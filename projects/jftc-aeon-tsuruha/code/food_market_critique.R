# =============================================================
#  追加節：食品小売市場での集中度分析（7県）— 2パス版
#  「スーパー不在商圏で、本件統合は食品小売の競争を実質的に制限するか」
# -------------------------------------------------------------
#  改良点：searchNearby は1タイル20件・ページング無しのため、食品とドラッグストアを
#  まとめて取ると密なタイルでドラッグストアが押し出される。そこで業態ごとにパスを分け、
#  ① スーパー（20件上限がほぼ効かない＝完全取得→スーパー不在判定が確実）
#  ② ドラッグストア（同上＝ツルハ系・イオン系を取りこぼさない）
#  ③ コンビニ（任意）を別々に取得して統合する。
#  さらに「スーパー不在商圏」を抽出し、商圏内ドラッグストアの所有者構成
#  （イオン系とツルハ系が重複しているか＝唯一の弊害候補か）を明示する。
# =============================================================

## 0. 設定 ----------------------------------------------------------
USE_GOOGLE        <- TRUE
NEAR_STEP         <- 0.05      # タイル間隔(度)。3パスでクレジット内に収めたい場合は 0.06 を推奨
FETCH_CONVENIENCE <- TRUE      # FALSE にするとコンビニを取得しない（約1/3節約、HHIへの影響は小）
SHOKEN_RADII      <- c(2000, 3000, 5000)
MAX_REQ_WARN      <- 16000
NEAR_RADIUS_M     <- ceiling(NEAR_STEP * 111000 * 0.72)   # セルを被覆する円半径(自動)
REUSE_DRUGSTORE_CSV <- ""      # 既存ドラッグストアCSV(name,lat,lon)があれば補完に使用

W_SUPERMARKET <- 1.0; W_WAREHOUSE <- 0.9; W_DISCOUNT <- 0.6
W_CONVENIENCE <- 0.15; W_DRUGSTORE <- 0.20

PREF_BBOX <- list(
  c(139.90,40.20,141.70,41.60), c(139.33,36.20,140.29,37.16),
  c(139.69,35.74,140.85,36.95), c(137.47,34.55,139.17,35.65),
  c(133.14,35.06,134.52,35.62), c(131.60,34.30,133.45,35.45),
  c(132.00,32.90,133.70,34.30))

pkgs <- c("sf","dplyr","stringr","leaflet","htmlwidgets","jsonlite","httr","readr","units","purrr")
new <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(new)) install.packages(new, repos="https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only=TRUE))
sf::sf_use_s2(TRUE)
`%||%` <- function(a,b) if (is.null(a)||length(a)==0) b else a

## 1. 分類（所有者・業態）------------------------------------------
AEON_PAT <- paste0(
  "イオン|マックスバリュ|ザ・?ビッグ|ビッグ・?エー|まいばすけっと|ダイエー|カスミ|マルエツ|",
  "マルナカ|山陽マルナカ|フジ|フジグラン|フジマート|光洋|ピーコック|いなげや|",
  "ウエルシア|ウェルシア|welcia|ハッピー|ハックドラッグ|ダックス|ウェルパーク|コクミン|ミニストップ")
TSURUHA_PAT <- paste0(
  "ツルハ|tsuruha|くすりのレデイ|レデイ|ウォンツ|wants|ウエルネス|ウェルネス|",
  "ドラッグイレブン|杏林堂|くすりの福太郎|b&d")
owner_of <- function(name){ low <- dplyr::coalesce(name,"")
  dplyr::case_when(
    str_detect(low, regex(TSURUHA_PAT, ignore_case=TRUE)) ~ "ツルハ系",
    str_detect(low, regex(AEON_PAT,    ignore_case=TRUE)) ~ "イオングループ",
    TRUE ~ paste0("競争者:", str_trunc(low,16)))}
biz_weight <- function(primary) dplyr::case_when(
  primary %in% c("supermarket","grocery_store","food_store") ~ W_SUPERMARKET,
  primary == "warehouse_store"                              ~ W_WAREHOUSE,
  primary %in% c("department_store","discount_store")        ~ W_DISCOUNT,
  primary == "convenience_store"                            ~ W_CONVENIENCE,
  primary %in% c("drugstore","pharmacy")                     ~ W_DRUGSTORE,
  TRUE ~ 0)
biz_label <- function(primary) dplyr::case_when(
  primary %in% c("supermarket","grocery_store","food_store") ~ "スーパー",
  primary == "warehouse_store"                              ~ "倉庫型",
  primary %in% c("department_store","discount_store")        ~ "ディスカウント",
  primary == "convenience_store"                            ~ "コンビニ",
  primary %in% c("drugstore","pharmacy")                     ~ "ドラッグストア",
  TRUE ~ "その他")

## 2. Google Places(New) searchNearby を業態パスごとに取得 --------
fetch_near <- function(lat, lng, radius_m, key, types){
  body <- list(includedTypes=types, maxResultCount=20L,
               locationRestriction=list(circle=list(
                 center=list(latitude=lat, longitude=lng), radius=radius_m)),
               languageCode="ja", regionCode="JP")
  r <- httr::POST("https://places.googleapis.com/v1/places:searchNearby",
        httr::add_headers("Content-Type"="application/json","X-Goog-Api-Key"=key,
          "X-Goog-FieldMask"="places.displayName,places.location,places.primaryType,places.types,places.businessStatus"),
        body=jsonlite::toJSON(body, auto_unbox=TRUE), encode="raw")
  if (httr::status_code(r) != 200) return(NULL)
  jr <- jsonlite::fromJSON(httr::content(r,"text",encoding="UTF-8"), simplifyVector=FALSE)
  pls <- jr$places %||% list(); if (!length(pls)) return(NULL)
  do.call(rbind, lapply(pls, function(p){
    if (is.null(p$location)) return(NULL)
    prim <- p$primaryType %||% (if (length(p$types)) p$types[[1]] else NA_character_)
    data.frame(name=p$displayName$text %||% "", lat=p$location$latitude, lon=p$location$longitude,
               primary=prim, status=p$businessStatus %||% "OPERATIONAL", stringsAsFactors=FALSE)
  }))
}
# 業態パス：スーパーとドラッグストアは別パス＝20件上限の影響を排除
PASSES <- list(`スーパー`=c("supermarket","grocery_store","warehouse_store"),
               `ドラッグストア`=c("drugstore","pharmacy"))
if (FETCH_CONVENIENCE) PASSES[["コンビニ"]] <- c("convenience_store")

food <- NULL
if (USE_GOOGLE) {
  key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  centers <- do.call(rbind, lapply(PREF_BBOX, function(b){
    lons <- seq(b[1],b[3],by=NEAR_STEP); lats <- seq(b[2],b[4],by=NEAR_STEP)
    expand.grid(lon=lons[-length(lons)]+NEAR_STEP/2, lat=lats[-length(lats)]+NEAR_STEP/2)
  }))
  total_req <- nrow(centers) * length(PASSES)
  cat(sprintf("[Google] %d タイル × %d パス = 約 %d リクエスト（Pro無料枠 5,000/月）\n",
              nrow(centers), length(PASSES), total_req))
  if (identical(key,"")) {
    message("※ GOOGLE_MAPS_API_KEY 未設定。Sys.setenv(GOOGLE_MAPS_API_KEY=\"...\") を先に実行。中止します。")
  } else {
    if (total_req > MAX_REQ_WARN)
      message(sprintf("※ 予測リクエストが %d を超えます。NEAR_STEP を 0.06 にするか FETCH_CONVENIENCE<-FALSE を検討。", MAX_REQ_WARN))
    parts <- list()
    for (pname in names(PASSES)) {
      cat(sprintf("  パス『%s』取得中…\n", pname))
      parts[[pname]] <- purrr::pmap_dfr(list(centers$lat, centers$lon),
        function(la,lo){ Sys.sleep(0.05); fetch_near(la, lo, NEAR_RADIUS_M, key, PASSES[[pname]]) })
    }
    food <- dplyr::bind_rows(parts)
    cat(sprintf("[Google] 取得 %d 件（重複・閉店含む）\n", nrow(food %||% data.frame())))
  }
}
if (is.null(food)) stop("食品データ未取得のため終了。")

## 3. 整形・分類・ウェイト ------------------------------------------
food <- food |>
  dplyr::filter(!is.na(lat), !is.na(lon), status=="OPERATIONAL") |>
  dplyr::mutate(rlat=round(lat,3), rlon=round(lon,3)) |>
  dplyr::distinct(rlat, rlon, name, .keep_all=TRUE) |> dplyr::select(-rlat,-rlon)
if (!identical(REUSE_DRUGSTORE_CSV,"") && file.exists(REUSE_DRUGSTORE_CSV)) {
  dd <- readr::read_csv(REUSE_DRUGSTORE_CSV, show_col_types=FALSE)
  if (all(c("name","lat","lon") %in% names(dd))) {
    dd <- dd |> dplyr::transmute(name, lat, lon, primary="drugstore", status="OPERATIONAL")
    food <- dplyr::bind_rows(food, dd) |>
      dplyr::mutate(rlat=round(lat,3), rlon=round(lon,3)) |>
      dplyr::distinct(rlat, rlon, name, .keep_all=TRUE) |> dplyr::select(-rlat,-rlon)
  }
}
food <- food |>
  dplyr::mutate(weight=biz_weight(primary), biz=biz_label(primary), owner=owner_of(name)) |>
  dplyr::filter(weight > 0)
readr::write_excel_csv(food, "shoken_food_raw.csv")   # 生データ保存（再実行時に再利用可）
cat("\n業態別 取得店舗数:\n"); print(table(food$biz))
cat(sprintf("当事会社側: イオングループ %d / ツルハ系 %d / 競争者 %d 店\n",
            sum(food$owner=="イオングループ"), sum(food$owner=="ツルハ系"),
            sum(startsWith(food$owner,"競争者"))))

## 4. ツルハ基点商圏ごとの食品HHI / ΔHHI --------------------------
fpts <- sf::st_as_sf(food, coords=c("lon","lat"), crs=4326, remove=FALSE)
B    <- dplyr::filter(food, owner=="ツルハ系")
Bpts <- sf::st_as_sf(B, coords=c("lon","lat"), crs=4326, remove=FALSE)
analyze_radius <- function(radius_m){
  nb <- sf::st_is_within_distance(Bpts, fpts, dist=units::set_units(radius_m,"m"))
  purrr::map_dfr(seq_len(nrow(B)), function(i){
    sub <- food[nb[[i]],]
    sup <- tapply(sub$weight, sub$owner, sum); sup <- sup[!is.na(sup)]
    tot <- sum(sup); sh <- 100*sup/tot
    hhi_pre <- sum(sh^2)
    own2 <- ifelse(names(sup) %in% c("イオングループ","ツルハ系"),"統合会社",names(sup))
    sup2 <- tapply(sup, own2, sum); sh2 <- 100*sup2/tot
    hhi_post <- sum(sh2^2); dhhi <- hhi_post - hhi_pre
    s_aeon <- if ("イオングループ" %in% names(sh)) sh[["イオングループ"]] else 0
    s_tsu  <- if ("ツルハ系" %in% names(sh)) sh[["ツルハ系"]] else 0
    party  <- if ("統合会社" %in% names(sh2)) sh2[["統合会社"]] else 0
    n_sm   <- sum(sub$biz %in% c("スーパー","倉庫型"))
    n_aeon_dg <- sum(sub$owner=="イオングループ" & sub$biz=="ドラッグストア")
    exceeds <- (hhi_post>2500 && dhhi>150) || (hhi_post>1500 && hhi_post<=2500 && dhhi>250)
    data.frame(name=B$name[i], lat=B$lat[i], lon=B$lon[i], radius_m=radius_m,
      supermarkets=n_sm, aeon_drugstores=n_aeon_dg,
      HHI_pre=round(hhi_pre), HHI_post=round(hhi_post), dHHI=round(dhhi),
      share_aeon=round(s_aeon,1), share_tsuruha=round(s_tsu,1), party_share=round(party,1),
      exceeds_safeharbor=exceeds,
      tier=dplyr::case_when(
        n_sm==0 & hhi_post>2500 & party>50 ~ "食品集中:重大(スーパー不在)",
        exceeds                            ~ "セーフハーバー超過(スーパー有=競争維持余地)",
        TRUE                               ~ "セーフハーバー内"))
  })
}
res_all <- purrr::map_dfr(SHOKEN_RADII, analyze_radius)
readr::write_excel_csv(res_all, "shoken_food_results.csv")
res2 <- dplyr::filter(res_all, radius_m==2000)
cat("\n=== 主分析(2km商圏) 区分別 ツルハ基点商圏数 ===\n"); print(table(res2$tier))
cat("\n半径別 超過数と『重大(スーパー不在)』数:\n")
print(res_all |> dplyr::group_by(radius_m) |>
  dplyr::summarise(超過=sum(exceeds_safeharbor),
                   重大_スーパー不在=sum(tier=="食品集中:重大(スーパー不在)"), .groups="drop"))

## 4b. スーパー不在商圏の精査（唯一の弊害候補＝イオン系の重複の有無）---
absent <- res2 |> dplyr::filter(supermarkets==0) |>
  dplyr::mutate(判定 = dplyr::case_when(
    aeon_drugstores>0 & share_tsuruha>0 ~ "★イオン系ドラッグと重複→弊害候補(要精査)",
    TRUE ~ "イオン系なし＝重複なし＝弊害なし"))
cat(sprintf("\n=== スーパー不在商圏(2km) %d 件の精査 ===\n", nrow(absent)))
if (nrow(absent)>0) print(absent |> dplyr::select(name, aeon_drugstores, share_aeon, share_tsuruha, HHI_post, 判定))
cat(sprintf("→ スーパー不在のうち イオン系ドラッグと重複（弊害候補）= %d 件\n",
            sum(absent$aeon_drugstores>0)))

## 5. 地図(2km・区分で色分け) -------------------------------------
tier_col <- function(t) dplyr::case_when(
  t=="食品集中:重大(スーパー不在)" ~ "#b2182b",
  t=="セーフハーバー超過(スーパー有=競争維持余地)" ~ "#fc8d59", TRUE ~ "#1a9850")
res2$col <- tier_col(res2$tier)
res2$popup <- sprintf("<b>%s</b><br>区分: %s<br>2km内スーパー: %d<br>統合後HHI: %d（増分 %d）<br>当事会社食品シェア: %.1f%%",
  res2$name, res2$tier, res2$supermarkets, res2$HHI_post, res2$dHHI, res2$party_share)
sm  <- dplyr::filter(food, biz %in% c("スーパー","倉庫型"))
cvs <- dplyr::filter(food, biz=="コンビニ")
W<-min(sapply(PREF_BBOX,`[`,1));S<-min(sapply(PREF_BBOX,`[`,2));E<-max(sapply(PREF_BBOX,`[`,3));N<-max(sapply(PREF_BBOX,`[`,4))
m <- leaflet() |> addProviderTiles("CartoDB.Positron") |> fitBounds(W,S,E,N) |>
  addCircleMarkers(data=cvs, lng=~lon, lat=~lat, radius=2, color="#bbbbbb", stroke=FALSE, fillOpacity=0.4,
    clusterOptions=markerClusterOptions(), group="コンビニ") |>
  addCircleMarkers(data=sm, lng=~lon, lat=~lat, radius=4, color="#2166ac", stroke=FALSE, fillOpacity=0.7,
    popup=~paste0(name," / ",owner), group="スーパー(青)") |>
  addCircleMarkers(data=res2, lng=~lon, lat=~lat, radius=~ifelse(tier=="食品集中:重大(スーパー不在)",8,5),
    color=~col, weight=1, fillOpacity=0.9, popup=~popup, group="ツルハ基点商圏(食品集中度)") |>
  addLayersControl(overlayGroups=c("ツルハ基点商圏(食品集中度)","スーパー(青)","コンビニ"),
    options=layersControlOptions(collapsed=FALSE)) |>
  addControl(position="bottomleft", html=paste0("<div style='background:white;padding:8px;font:12px sans-serif'>",
    "<b>食品小売の集中度（ツルハ基点2km）</b><br>",
    "<span style='color:#b2182b'>●</span>重大:スーパー不在で統合後高HHI<br>",
    "<span style='color:#fc8d59'>●</span>SH超過(スーパー有)<span style='color:#1a9850'> ●</span>SH内</div>"))
htmlwidgets::saveWidget(m, "shoken_map_food.html", selfcontained=TRUE)
cat("\n出力: shoken_food_results.csv / shoken_food_raw.csv / shoken_map_food.html\n")
