前提：仕様を曖昧にすると、`md-preview` と `md-site` で出力がズレて破綻する。**単一の真実は md-core**。preview/site は“使い方”だけ違う。

以下、具体的な入出力（契約）案。

---

## 1) md-core：Compiler / Transform Engine

### 入力

**A. ソース**

* `markdown: string`
* `id?: string`（ファイルパスやURLなど。診断やリンク解決のキー）
* `frontmatter?: Record<string, any>`（外で抽出して渡す or coreで抽出して返す）

**B. 環境**

* `env: "server" | "browser"`（分岐が必要なら）
* `baseUrl?: string`（相対リンク解決）
* `path?: string`（相対リンク解決や見出しIDの安定化に使う）

**C. オプション（決定的に）**

* `features: { gfm: boolean; footnotes: boolean; math: boolean; mermaid: boolean; rawHtml: "allow"|"deny"|"passthrough"; }`
* `target: "html" | "hast" | "mdast" | "ast-json"`（少なくとも html と ast は欲しい）
* `plugins?: Plugin[]`（後述）

### 出力

`CompileResult`

* `html?: string`
* `ast?: Ast`（mdast/hast等。どれかに統一）
* `metadata: {`

  * `frontmatter?: Record<string, any>`
  * `headings: Array<{ depth: 1|2|3|4|5|6; text: string; id: string; }> `
  * `links: Array<{ href: string; text?: string; position?: Range; }> `
  * `images: Array<{ src: string; alt?: string; position?: Range; }> `
  * `wordCount?: number; readingTimeSec?: number`
  * `toc?: TocNode[]`（headingsから作るならここ）
* `diagnostics: Array<{ severity: "error"|"warn"|"info"; code: string; message: string; position?: Range; hint?: string; }>`
* `dependencies?: { files?: string[]; urls?: string[] }`（siteの増分ビルド用）
* `sourceMap?: Mapping[]`（previewのクリック→行ジャンプ/スクロール同期で使うなら）

### プラグイン契約（最低限）

* 入力/出力段階を固定（ズレ防止）

  * `remark`段階（md -> mdast）
  * `rehype`段階（mdast -> hast）
  * `render`段階（hast -> html）
* `Plugin`は「どの段階で何を受け取り何を返すか」を型で縛る。

---

## 2) md-preview：Editor + Live Preview UI

### 入力

`PreviewProps / PreviewConfig`

* `initialMarkdown?: string`
* `value?: string`（controlled）
* `onChange?: (md: string) => void`
* `compiler: { compile(markdown, options) => CompileResult }`（md-coreの薄い依存）
* `compileOptions?: MdCoreOptions`（coreへそのまま渡す）
* `assets?: { resolveUrl?: (url: string, ctx) => string }`（画像/リンク解決をUI側で上書き）
* `security?: { sandbox?: boolean; sanitize?: "strict"|"none"; allowedTags?: string[] }`
* `ui?: { theme?: "light"|"dark"; toolbar?: boolean; }`

### 出力（UIとしてのイベント/状態）

* `onDiagnostics?: (diags: CompileResult["diagnostics"]) => void`
* `onNavigate?: (href: string, meta) => void`（リンククリック時）
* `onScrollSync?: (pos) => void`（必要なら）
* `getSnapshot?: () => { markdown: string; html: string; ast?: Ast; }`（外部保存や共有用）

### “プレビュー専用”の追加出力（重要）

* `mapping`（ソース行 ↔ DOM位置）を使うなら

  * md-coreが `sourceMap` を返す
  * md-previewがDOMへ `data-sourcepos` を埋め込み、同期を実現
* ここは **md-coreの契約に含めるか、preview専用pluginとしてcoreに注入**。隠し実装にしない。

---

## 3) md-site：Publish / SSG(or SSR) App + CLI

### 入力

**A. コンテンツソース**

* `contentDir: string`（例: `docs/`）
* `include: string[]` / `exclude: string[]`
* `route: (filePath) => { slug: string; lang?: string; }`
* `frontmatterSchema?: zod/jsonschema`（検証して落とす）

**B. ビルド設定**

* `outDir: string`
* `baseUrl: string`
* `site: { title; description; locale; }`
* `theme: { layoutTemplates; css; components; }`

**C. md-core統合**

* `compiler: md-core`（固定）
* `compileOptions: MdCoreOptions`（サイト全体で固定。previewと同じにする）
* `linkPolicy: { brokenLink: "error"|"warn"|"ignore"; }`
* `assets: { copy?: boolean; optimizeImages?: boolean; }`

### 出力

**A. 生成物**

* `dist/`（HTML/CSS/JS/assets）
* `manifest.json`（ページ一覧、ハッシュ、依存、検索インデックス）
* `sitemap.xml` / `rss.xml`（任意）

**B. ビルド結果（プログラム/CLIの戻り値）**
`BuildResult`

* `pages: Array<{ slug: string; title?: string; sourcePath: string; outPath: string; }> `
* `diagnostics: Diagnostic[]`（md-coreの+サイト固有）
* `brokenLinks: Array<{ from: slug; href: string; }> `
* `incremental: { changed: string[]; rebuilt: string[]; }`

**C. 開発サーバ**

* `dev()` が返す

  * `port`
  * `invalidate(path)`（ファイル更新時の再ビルド）
  * `close()`

---

## 仕様の“固定点”（揉めないための鉄則）

* `MdCoreOptions` を **md-preview と md-site で共有**（同一JSONで表現可能にする）
* `CompileResult.metadata` は **siteのナビ/検索/TOC** と **previewのアウトライン** の共通基盤にする
* 例外的な差分（previewのsourceMapなど）は「preview用plugin」として md-core に注入し、隠し分岐にしない

---

必要なら、この仕様をそのまま **TypeScriptの型定義（interfaces）** として起こして、各パッケージの依存方向が崩れない形（`@md/types` 的な共通型を切る/切らないの判断含む）まで落とせる。

