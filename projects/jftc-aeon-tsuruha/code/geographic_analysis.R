# ============================================================
#  7県版: 懸念商圏のある7県を OSM + Google Places で精査
#  対象: 青森・栃木・茨城・静岡・鳥取・島根・愛媛
# ------------------------------------------------------------
#  各県の矩形範囲で OSM(全件)を取得し、任意で Google Places で補完して
#  被覆を上げ、2km商圏分析(競合商圏・懸念商圏)を行い地図化する。
#  ・Google は USE_GOOGLE <- TRUE + 環境変数 GOOGLE_MAPS_API_KEY が必要(有料)。
#    7県(0.05°)で約5,000リクエスト ≒ Proの月間無料枠5,000の境界。
#    確実に無料枠内に収めたい場合は GOOGLE_STEP <- 0.06 にする(タイル約3割減)。
#    料金・無料枠は変動するため公式ページとコンソールの予算上限で必ず確認。
# ============================================================

## 0. 設定 ----------------------------------------------------------
USE_GOOGLE  <- TRUE                 # FALSE なら OSM のみ(無料)
GOOGLE_STEP <- 0.05                 # 補完タイルの一辺(度)。0.06で約3割削減=確実に無料枠内
MAX_REQ_WARN <- 6000                # 予測リクエストがこれを超えたら警告

PREF_NAMES <- c("青森県","栃木県","茨城県","静岡県","鳥取県","島根県","愛媛県")
PREF_BBOX  <- list(                 # c(西経度, 南緯度, 東経度, 北緯度) ※島根=本土のみ
  c(139.90,40.20,141.70,41.60), c(139.33,36.20,140.29,37.16),
  c(139.69,35.74,140.85,36.95), c(137.47,34.55,139.17,35.65),
  c(133.14,35.06,134.52,35.62), c(131.60,34.30,133.45,35.45),
  c(132.00,32.90,133.70,34.30))

pkgs <- c("sf","dplyr","stringr","leaflet","htmlwidgets","jsonlite","httr","readr","units")
new <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))
sf::sf_use_s2(TRUE)
`%||%` <- function(a, b) if (is.null(a)) b else a

BIG <- paste0("ウエルシア|ウェルシア|welcia|ツルハ|tsuruha|マツモトキヨシ|マツキヨ|matsukiyo|ココカラ|スギ|",
  "サンドラッグ|クスリのアオキ|くすりのアオキ|アオキ|コスモス|クリエイト|ゲンキー|ダイコク|キリン堂|サツドラ|",
  "サッポロドラッグ|セイムス|セイジョー|トモズ|tomod|セガミ|ザグザグ|ユタカ|薬王堂|くすりの福太郎|ウォンツ|wants|",
  "ウエルネス|ウェルネス|ドラッグイレブン|杏林堂|レデイ|ハッピー|ひまわり|ダックス|ウェルパーク|コクミン|",
  "ハックドラッグ|新生堂|大賀|ミドリ薬品|ぱぱす|スギヤマ|中部薬品|ドラッグ|ドラックス|どらっぐ|ドラッグストア|drugstore")

## 1. OSM を県ごとに取得(失敗時は同梱CSVを範囲で絞り込み) ----------
fetch_osm_bbox <- function(b) {  # b = c(west,south,east,north)
  q <- sprintf('[out:json][timeout:120];(
      node["shop"="chemist"](%f,%f,%f,%f);way["shop"="chemist"](%f,%f,%f,%f);
      node["shop"="pharmacy"][~"^(name|brand)$"~"%s"](%f,%f,%f,%f);
      way["shop"="pharmacy"][~"^(name|brand)$"~"%s"](%f,%f,%f,%f);
      node["amenity"="pharmacy"][~"^(name|brand)$"~"%s"](%f,%f,%f,%f);
      way["amenity"="pharmacy"][~"^(name|brand)$"~"%s"](%f,%f,%f,%f););out center tags;',
      b[2],b[1],b[4],b[3], b[2],b[1],b[4],b[3], BIG,b[2],b[1],b[4],b[3], BIG,b[2],b[1],b[4],b[3],
      BIG,b[2],b[1],b[4],b[3], BIG,b[2],b[1],b[4],b[3])
  resp <- httr::POST("https://overpass-api.de/api/interpreter", body=list(data=q), encode="form",
            httr::user_agent("shoken-seminar-demo/1.0 (research)"), httr::timeout(160))
  httr::stop_for_status(resp)
  els <- jsonlite::fromJSON(httr::content(resp,"text",encoding="UTF-8"), simplifyVector=FALSE)$elements
  do.call(rbind, lapply(els, function(el){
    tg<-el$tags %||% list(); lat<-if(!is.null(el$lat)) el$lat else el$center$lat
    lon<-if(!is.null(el$lon)) el$lon else el$center$lon
    if(is.null(lat)||is.null(lon)) return(NULL)
    data.frame(name=(tg$brand %||% tg$name) %||% NA_character_, lat=lat, lon=lon, src="OSM")}))
}
message("OSM取得中(7県)…")
osm <- tryCatch(do.call(rbind, lapply(PREF_BBOX, function(b){Sys.sleep(1); fetch_osm_bbox(b)})),
                error=function(e) NULL)
if (is.null(osm) || nrow(osm)==0) {
  message("Overpass失敗。jp_drugstores.csv を7県範囲で使用。")
  cc <- readr::read_csv("jp_drugstores.csv", show_col_types=FALSE)
  inb <- Reduce(`|`, lapply(PREF_BBOX, function(b) cc$lon>=b[1]&cc$lon<=b[3]&cc$lat>=b[2]&cc$lat<=b[4]))
  osm <- dplyr::mutate(cc[inb,], src="OSM")
}

## 2.【任意】Google Places(New)で補完 ------------------------------
fetch_google_bbox <- function(b, step, key) {
  lons<-seq(b[1],b[3],by=step); lats<-seq(b[2],b[4],by=step); out<-list()
  for(i in seq_len(length(lons)-1)) for(j in seq_len(length(lats)-1)){
    rect<-list(low=list(latitude=lats[j],longitude=lons[i]),
               high=list(latitude=lats[j+1],longitude=lons[i+1])); token<-NULL
    repeat{
      body<-list(textQuery="ドラッグストア", pageSize=20, languageCode="ja", regionCode="JP",
                 locationRestriction=list(rectangle=rect))
      if(!is.null(token)) body$pageToken<-token
      r<-httr::POST("https://places.googleapis.com/v1/places:searchText",
        httr::add_headers("Content-Type"="application/json","X-Goog-Api-Key"=key,
          "X-Goog-FieldMask"="places.displayName,places.location,nextPageToken"),
        body=jsonlite::toJSON(body,auto_unbox=TRUE), encode="raw")
      jr<-jsonlite::fromJSON(httr::content(r,"text",encoding="UTF-8"), simplifyVector=FALSE)
      for(p in (jr$places %||% list()))
        out[[length(out)+1]]<-data.frame(name=(p$displayName$text %||% ""),
          lat=p$location$latitude, lon=p$location$longitude, src="Google")
      token<-jr$nextPageToken %||% NULL; if(is.null(token)) break; Sys.sleep(2)
    }; Sys.sleep(0.2)
  }
  if(length(out)) do.call(rbind,out) else NULL
}
df <- osm
if (USE_GOOGLE) {
  key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  tiles <- sum(sapply(PREF_BBOX, function(b) (length(seq(b[1],b[3],by=GOOGLE_STEP))-1)*
                                             (length(seq(b[2],b[4],by=GOOGLE_STEP))-1)))
  cat(sprintf("[Google] 予測タイル数 %d → 約%d〜%d リクエスト(無料枠 Pro 5,000/月)\n",
              tiles, tiles, round(tiles*1.2)))
  if (identical(key,"")) {
    message("※ GOOGLE_MAPS_API_KEY 未設定のため Google 補完をスキップ(OSMのみで実行)。")
  } else {
    if (round(tiles*1.2) > MAX_REQ_WARN)
      message(sprintf("※ 予測リクエストが %d を超えます。GOOGLE_STEP を大きくするか県を分けて実行を。", MAX_REQ_WARN))
    g <- tryCatch(do.call(rbind, lapply(PREF_BBOX, fetch_google_bbox, step=GOOGLE_STEP, key=key)),
                  error=function(e){message(e); NULL})
    if(!is.null(g)){ cat(sprintf("[Google] %d 件取得\n", nrow(g))); df <- dplyr::bind_rows(osm, g) }
  }
}

## 3. 結合 → 重複除去(同一グループ・約110m以内を1店に) ----------------
A_pat <- "welcia|ウエルシア|ウェルシア|イオン|ダックス|ウェルパーク|コクミン|ハックドラッグ|ハッピー|ふく薬品|よどや|マルエドラッグ|ひまわり"
B_pat <- "ツルハ|tsuruha|レデイ|ウォンツ|wants|ウエルネス|ウェルネス|ドラッグイレブン|杏林堂|くすりの福太郎|b&d"
C_pat <- "マツモトキヨシ|マツキヨ|ココカラ|スギ|サンドラッグ|アオキ|コスモス|クリエイト|ゲンキー|ダイコク|キリン堂|サツドラ|セイムス|セイジョー|トモズ|tomod|セガミ|ザグザグ|ユタカ|薬王堂|新生堂|大賀|ミドリ薬品|ぱぱす|スギヤマ|中部薬品|ドラッグ"
grp <- function(nm){low<-tolower(dplyr::coalesce(nm,""));raw<-dplyr::coalesce(nm,"")
  dplyr::case_when(str_detect(low,tolower(A_pat))~"A_イオン系",str_detect(low,tolower(B_pat))~"B_ツルハ系",
    str_detect(low,tolower(C_pat))~"競争者DgS",str_detect(raw,"薬局")~"調剤薬局",TRUE~"競争者DgS")}
chain <- function(nm){low<-tolower(dplyr::coalesce(nm,""))
  dplyr::case_when(str_detect(low,"マツモトキヨシ|マツキヨ")~"マツキヨ",str_detect(low,"ココカラ")~"ココカラ",
    str_detect(low,"スギ")~"スギ",str_detect(low,"サンドラッグ")~"サンドラッグ",str_detect(low,"アオキ")~"クスリのアオキ",
    str_detect(low,"コスモス")~"コスモス",str_detect(low,"クリエイト")~"クリエイト",str_detect(low,"ゲンキー")~"ゲンキー",
    str_detect(low,"サツドラ")~"サツドラ",str_detect(low,"セイムス")~"セイムス",str_detect(low,"トモズ")~"トモズ",
    str_detect(low,"薬王堂")~"薬王堂",str_detect(low,"中部薬品")~"Vドラッグ",TRUE~paste0("その他(",substr(low,1,8),")"))}
df <- df |> dplyr::filter(!is.na(lat),!is.na(lon)) |>
  dplyr::mutate(group=grp(name), chain=chain(name)) |> dplyr::filter(group!="調剤薬局") |>
  dplyr::mutate(rlat=round(lat,3), rlon=round(lon,3)) |>            # 約110mで同一グループ重複を統合
  dplyr::distinct(rlat, rlon, group, .keep_all=TRUE) |> dplyr::select(-rlat,-rlon)
cat(sprintf("7県 統合後 %d 店 (OSM %d / Google %d)\n", nrow(df),
            sum(df$src=="OSM"), sum(df$src=="Google")))
print(table(df$group))

## 4. 2km商圏分析 ---------------------------------------------------
pts<-st_as_sf(df,coords=c("lon","lat"),crs=4326,remove=FALSE)
A<-pts[pts$group=="A_イオン系",];B<-pts[pts$group=="B_ツルハ系",];C<-pts[pts$group=="競争者DgS",]
d2<-units::set_units(2000,"m");nbA<-st_is_within_distance(B,A,dist=d2);nbC<-st_is_within_distance(B,C,dist=d2)
res<-B|>st_drop_geometry()|>dplyr::transmute(name,lat,lon,A_in_2km=lengths(nbA),
  competitor_groups_2km=vapply(seq_len(nrow(B)),function(i) length(unique(C$chain[nbC[[i]]])),integer(1)),
  comp_stores_2km=lengths(nbC))|>
  dplyr::mutate(
    is_competing_shoken = A_in_2km >= 1,                # イオン系と重複=企業結合が市場構造を変える
    is_concern = is_competing_shoken & competitor_groups_2km <= 1,
    # 企業結合との因果関係で区分:
    #  重複なし     … イオン系が2km内に無い→結合の前後で構造不変→因果なし(懸念から除外)
    #  競争維持     … 重複あり+他の競争者2グループ以上→結合後も競争維持
    #  懸念:複占化  … 重複あり+他の競争者1→結合で当事会社が2→1に減り複占化
    #  懸念:独占化  … 重複あり+他の競争者0→結合で独占化(最も強い懸念)
    category = dplyr::case_when(
      A_in_2km == 0              ~ "重複なし(企業結合の影響なし)",
      competitor_groups_2km == 0 ~ "懸念:独占化(結合後の競争者0)",
      competitor_groups_2km == 1 ~ "懸念:複占化(結合後の競争者1)",
      TRUE                       ~ "競争維持(競争者2グループ以上)"))
cat("\n7県 区分別 B系店舗数(企業結合との因果関係で分類):\n"); print(table(res$category))
cat(sprintf("→ 企業結合が原因の懸念(重複あり&他競争者<=1) %d 件 / 重複なし(因果なし) %d 件\n",
            sum(res$is_concern), sum(res$A_in_2km==0)))
readr::write_excel_csv(res,"shoken_7pref_results.csv")

## 5. 地図(7県にフィット。色=企業結合との因果関係の区分) -----------
cat_col <- function(cat) dplyr::case_when(
  cat=="懸念:独占化(結合後の競争者0)" ~ "#b2182b",   # 濃赤
  cat=="懸念:複占化(結合後の競争者1)" ~ "#ef8a62",   # 橙
  cat=="競争維持(競争者2グループ以上)" ~ "#1a9850",   # 緑
  TRUE                                 ~ "#9aa0a6")   # 灰(重複なし)
res$col<-cat_col(res$category)
res$popup<-sprintf("<b>%s</b><br>区分: %s<br>2km内のイオン系: %d<br>他の競争者グループ: %d",
  res$name, res$category, res$A_in_2km, res$competitor_groups_2km)
res_overlap<-dplyr::filter(res, is_competing_shoken)    # 重複あり=企業結合が影響
res_none   <-dplyr::filter(res, !is_competing_shoken)   # 重複なし=因果なし
df_C<-dplyr::filter(df,group=="競争者DgS");df_A<-dplyr::filter(df,group=="A_イオン系")
W<-min(sapply(PREF_BBOX,`[`,1));S<-min(sapply(PREF_BBOX,`[`,2));E<-max(sapply(PREF_BBOX,`[`,3));N<-max(sapply(PREF_BBOX,`[`,4))
m<-leaflet()|>addProviderTiles("CartoDB.Positron")|>fitBounds(W,S,E,N)|>
  addCircleMarkers(data=df_C,lng=~lon,lat=~lat,radius=3,color="#999999",stroke=FALSE,fillOpacity=0.5,
    clusterOptions=markerClusterOptions(),group="競争者ドラッグストア")|>
  addCircleMarkers(data=df_A,lng=~lon,lat=~lat,radius=3,color="#d7301f",stroke=FALSE,fillOpacity=0.6,group="A系(イオン系)")|>
  addCircleMarkers(data=res_none,lng=~lon,lat=~lat,radius=2,color="#9aa0a6",stroke=FALSE,fillOpacity=0.4,
    popup=~popup,group="重複なし(企業結合の影響なし)")|>
  addCircleMarkers(data=res_overlap,lng=~lon,lat=~lat,radius=~ifelse(competitor_groups_2km<=1,7,4),
    color=~col,weight=1,fillOpacity=0.9,popup=~popup,group="重複あり(色=結合後の市場構造)")|>
  addLayersControl(overlayGroups=c("重複あり(色=結合後の市場構造)","重複なし(企業結合の影響なし)","A系(イオン系)","競争者ドラッグストア"),
    options=layersControlOptions(collapsed=FALSE))|>
  addControl(position="bottomleft",html=paste0("<div style='background:white;padding:8px;font:12px sans-serif'>",
    "<b>B系店舗の区分(企業結合との因果)</b><br>",
    "<span style='color:#b2182b'>●</span>懸念:独占化 <span style='color:#ef8a62'>●</span>懸念:複占化<br>",
    "<span style='color:#1a9850'>●</span>競争維持 <span style='color:#9aa0a6'>●</span>重複なし(影響なし)</div>"))
htmlwidgets::saveWidget(m,"shoken_map_7pref.html",selfcontained=TRUE)
cat("地図 shoken_map_7pref.html を出力しました。\n")
