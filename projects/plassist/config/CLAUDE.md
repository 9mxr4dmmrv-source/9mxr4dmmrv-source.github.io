# Plassist 開発ワークスペース

このリポジトリは Plassist（予定時間と実績時間を可視化する時間管理アプリ）の
開発・運用用。Claude Code は Plassist プロジェクトのルートで起動する。

## 技術スタック（事実）
- React Native / Expo（EAS Build / OTA アップデート / TestFlight）
- 公開: App Store（既存）＋ Google Play（新規: デベロッパーアカウント取得済み）
- サポートサイト: Vite + React（GitHub Pages）

## リポジトリの境界（棚卸し済み・2026-08）
- 本体は client/ のみ。エントリは package.json "main": "client/index.js" → client/App.tsx。
  @/ は ./client/* に解決される（babel.config.js と tsconfig.json の両方）。
- データ層は client/lib/storage.ts（AsyncStorage）に完結。サーバーAPIへの実通信は無い。
- 次は旧Replitテンプレートの名残で、client/ から一切参照されていない。読むのも触るのも不要:
  server/ shared/ drizzle.config.ts .replit scripts/build.js dist/
- 死んだ配線（現状どこからも呼ばれていない。改修のついでに触らない）:
  client/lib/query-client.ts, client/navigation/MainTabNavigator26.tsx
- 上記を変更・削除する提案が必要になったら、実行せず理由を添えて永田に確認する。

## 事実のソース
- コードの真実はこのリポジトリの現物。記憶や推測で API・画面・挙動を断定しない。
  分からなければ読んで確かめる。
- マーケの数値の真実は marketing/data/ に置いた実データ
  （App Store Connect / Play Console のエクスポート等）。
  無い数字を創作しない。足りなければ「データ不足」と明記して止める。

## 出力の規約
- コードの変更は最小差分で。無関係な整形・リファクタを混ぜない。
- 生成物は指定パスに書く。既存ファイルを上書きする前に一言確認する。
- 破壊的な操作（リリース申請・ストア公開・ファイル削除・鍵/署名の操作）は
  実行せず、手順を提示して人間（永田）に委ねる。

## OTAアップデートの配信ルール（2026-08-17のインシデントで確立）
- 背景: 5月ビルド（v1.0.12 build14）の本体に対して、8月に検証目的で
  `eas update --branch production` を直接叩いてしまい、依存ライブラリのバージョンが
  ズレたJSが配信された。TestFlightとApp Storeは同じ production チャンネル／同じ
  runtimeVersion を見ているため、両方の実機で起動直後クラッシュが発生した。
  `eas update:roll-back-to-embedded` で復旧済み。
- 再発防止1（構造的対策・導入済み）: `app.json` の `runtimeVersion` はネイティブ変更を
  自動検知する `{"policy": "fingerprint"}` に変更済み。ネイティブに影響する依存が
  変わると自動でバージョンが変わるため、本体と噛み合わないJSはそもそも配信されなくなる。
  ただし既存の配布済みビルド（App Store / 提出済みAndroid）には遡って効かない。
  次にビルドし直したタイミングから有効。
- 再発防止2（運用ルール）: `eas update --branch production` を検証目的で直接叩かない。
  検証したいJS変更は必ず `eas update --branch preview` に先に配信し、preview チャンネルの
  ビルド（もしくは開発ビルド）で動作確認してから production へ昇格する。
  「実機でサッと試したいだけ」でも production を経由しない。

<!--
注意: このファイルは全サブエージェント（pg-*, mk-*）に自動で読み込まれる。
戦略・設計方針（何を作るか / どう差別化するか / どのマーケ施策を打つか）はここに書かない。
レビュー系（pg-reviewer / mk-critic）に漏れると、判断が「設計通りか」に引っ張られてバイアスになる。
設計方針は design-rules-pg.md / design-rules-mk.md に置き、メインで設計するときだけ明示的に参照する。
-->
