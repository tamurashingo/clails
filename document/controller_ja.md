# clails Controller ガイド

## 概要

clails の Controller は Ruby on Rails の Controller を参考にした HTTP リクエストハンドラーです。
Controller は HTTP リクエストを受け取り、Model を使ってデータを処理し、View にデータを渡したり、JSON レスポンスを返したりします。

## 基本概念

- Controller は HTTP メソッド（GET、POST、PUT、DELETE）ごとに処理を定義します
- アクションベースのルーティングにより、1つの Controller で複数のアクション（index, show, new, edit など）を定義できます
- `resources` 関数を使って、RESTful なルーティングを簡潔に定義できます
- Web アプリケーション用の `<web-controller>` と REST API 用の `<rest-controller>` があります
- ルーティングテーブルで URL パスと Controller を紐付けます
- URL パラメータは自動的に抽出され、Controller からアクセスできます

---

## 1. Controller の種類

clails には3種類の Controller クラスがあります。

### `<base-controller>`

すべての Controller の基底クラスです。HTTP リクエストの基本的な処理を提供します。

```common-lisp
(defclass <my-controller> (<base-controller>)
  ())
```

### `<web-controller>`

HTML ビューをレンダリングする Web アプリケーション用の Controller です。

```common-lisp
(defclass <my-web-controller> (<web-controller>)
  ())
```

### `<rest-controller>`

JSON などの構造化データを返す REST API 用の Controller です。

```common-lisp
(defclass <my-api-controller> (<rest-controller>)
  ())
```

---

## 2. Controller の定義

### 基本的な Controller 定義

```common-lisp
(in-package #:your-app/controller)

(defclass <users-controller> (<web-controller>)
  ()
  (:documentation "Users controller for managing user resources"))
```

### HTTP メソッドハンドラの実装

> **非推奨:** `do-get` / `do-post` / `do-put` / `do-delete` は clails のもともとの、HTTP メソッドに基づくディスパッチ機構です。以下の説明どおり今後も動作し続け、このリリースで削除されることはありませんが、新しく書くコードでは代わりに[アクションベースのルーティング](#アクションベースのルーティング)を使うことを推奨します。理由と移行方法は後述の[「HTTP メソッドディスパッチから `:action` ベースのルーティングへの移行」](#http-メソッドディスパッチから-action-ベースのルーティングへの移行)を参照してください。

各 HTTP メソッドに対応するメソッドをオーバーライドします。

```common-lisp
;; GET リクエストの処理
(defmethod do-get ((controller <users-controller>))
  (let ((users (get-all-users)))
    (set-view controller "users/index.html" 
              `(:users ,users))))

;; POST リクエストの処理
(defmethod do-post ((controller <users-controller>))
  (let* ((name (param controller "name"))
         (email (param controller "email"))
         (user (create-user name email)))
    (set-redirect controller "/users")))

;; PUT リクエストの処理
(defmethod do-put ((controller <users-controller>))
  (let* ((id (param controller "id"))
         (name (param controller "name"))
         (user (update-user id name)))
    (set-view controller "users/show.html"
              `(:user ,user))))

;; DELETE リクエストの処理
(defmethod do-delete ((controller <users-controller>))
  (let ((id (param controller "id")))
    (delete-user id)
    (set-redirect controller "/users")))
```

### HTML フォームから PUT・DELETE リクエストを送信する

HTML の `<form>` タグは `GET` と `POST` メソッドしかサポートしていません。
PUT や DELETE リクエストを HTML フォームから送信するには、`_method` パラメータを使用します。

**PUT リクエストの例：**

```html
<form action="/users/123" method="POST">
    <input type="hidden" name="_method" value="PUT">
    <input type="text" name="name" value="John Doe">
    <button type="submit">更新</button>
</form>
```

**DELETE リクエストの例：**

```html
<form action="/users/123" method="POST">
    <input type="hidden" name="_method" value="DELETE">
    <button type="submit">削除</button>
</form>
```

clails は POST リクエスト内の `_method` パラメータをチェックし、以下のようにルーティングします：

- `_method` が `"PUT"` の場合 → `do-put` メソッドを呼び出し
- `_method` が `"DELETE"` の場合 → `do-delete` メソッドを呼び出し
- `_method` が指定されていない場合 → `do-post` メソッドを呼び出し

これにより、HTML フォームから REST API のような操作が可能になります。

---

## 3. ルーティングの設定

ルーティングテーブルで URL パスと Controller を紐付けます。

### ルーティングテーブルの定義

`config/routes.lisp` などでルーティングを定義します。

```common-lisp
(in-package #:your-app/config)

(setf clails/environment:*routing-tables*
  '(;; トップページ
    (:path "/"
     :controller "your-app/controller::<top-controller>")
    
    ;; ユーザー一覧・作成
    (:path "/users"
     :controller "your-app/controller::<users-controller>")
    
    ;; ユーザー詳細・更新・削除
    (:path "/users/:id"
     :controller "your-app/controller::<user-controller>")
    
    ;; ネストしたリソース
    (:path "/users/:user-id/posts/:post-id"
     :controller "your-app/controller::<user-posts-controller>")))

;; アプリケーション起動時に初期化
(clails/controller/base-controller:initialize-routing-tables)
```

### URL パラメータの抽出

URL パス内の `:parameter-name` は自動的に抽出され、`param` 関数でアクセスできます。

```common-lisp
;; ルート定義: "/users/:user-id/posts/:post-id"
;; アクセス例: GET /users/123/posts/456

(defmethod do-get ((controller <user-posts-controller>))
  (let ((user-id (param controller "user-id"))   ; => "123"
        (post-id (param controller "post-id")))  ; => "456"
    ;; 処理...
    ))
```

### アクションベースのルーティング

ルートに `:action` と `:method` を指定することで、1つの Controller に複数のアクションメソッドを定義できます。

```common-lisp
(setf clails/environment:*routing-tables*
  '((:path "/todos"
     :controller "your-app/controllers/todo-controller::<todo-controller>"
     :action "index"
     :method :get)
    (:path "/todos/:id"
     :controller "your-app/controllers/todo-controller::<todo-controller>"
     :action "show"
     :method :get)
    (:path "/todos/new"
     :controller "your-app/controllers/todo-controller::<todo-controller>"
     :action "new"
     :method :get)))
```

対応する Controller では、`do-get` の代わりにアクション名のメソッドを定義します。

```common-lisp
(defclass <todo-controller> (<web-controller>)
  ())

(defmethod index ((controller <todo-controller>))
  (set-view controller "todos/index.html"))

(defmethod show ((controller <todo-controller>))
  (let ((id (param controller "id")))
    (set-view controller "todos/show.html" `(:id ,id))))

(defmethod new ((controller <todo-controller>))
  (set-view controller "todos/new.html"))
```

**ルートマッチングの優先順位:**

1. パスとHTTPメソッドが両方一致するルート（最優先）
2. パスのみ一致し、`:method` が指定されていないルート
3. マッチなし → 404

**後方互換性:** `:action` が指定されていないルートは、従来通り `do-get`、`do-post` 等にディスパッチされます。この HTTP メソッドディスパッチ機構は非推奨です（後述）が、このリリースで削除されることはありません。

### HTTP メソッドディスパッチから `:action` ベースのルーティングへの移行

clails には、ルートがどの Controller メソッドを呼ぶかを決める方法が現在2つあります。

1. **HTTP メソッドディスパッチ**（もともとの機構）: ルートには `:path` と `:controller` のみを指定し、ミドルウェア（`clails/middleware/clails-middleware`）がリクエストの HTTP メソッドに基づいて `do-get`、`do-post`、`do-put`、`do-delete` のいずれかを選びます。POST リクエストに対する `_method` パラメータのチェックも含まれます（後述）。
2. **`:action` ベースのルーティング**（前述）: ルートに `:path`、`:method`、`:controller`、`:action` を指定し、フレームワークが `:action` で指定されたメソッドを直接呼び出します。

両者は現在どちらも動作しますが、**新しいコードでは `:action` ベースのルーティングを使うことを推奨**し、既存の HTTP メソッドディスパッチのルートは徐々に移行してください。

- **パス → メソッドの対応関係が一箇所にまとまる。** HTTP メソッドディスパッチでは、あるリクエストで実際にどの Controller メソッドが呼ばれるかは、ルーティングテーブル（どの Controller がそのパスを処理するか）とミドルウェアの HTTP メソッド用 `cond`（`do-get`/`do-post`/`do-put`/`do-delete` のどれを呼ぶか、および `_method` による上書き）という2箇所に分かれて決まります。`:action` ベースのルーティングでは、1つのルートエントリの `:path`、`:method`、`:action` だけがその対応関係の唯一の定義場所になります。
- **正しいディスパッチ先に到達するために `_method` の偽装が不要。** `path-controller` は `:action` ルートをリクエストの実際の HTTP メソッドと直接照合し、`_method` パラメータは一切参照しません。このチェックが存在するのは、ミドルウェアの従来（レガシー）フォールバック分岐だけです。そのため、Controller に新しいアクションを追加するときに `_method` のケースを追加で配線する必要がありません。
- **1つのメソッドに複数の意味を持たせずに、Controller ごとに複数のアクションを定義できる。** `do-get` は Controller ごとに1つの意味しか持てません。`:action` を使えば、`resources` 関数がすでに前提としているように、`index`、`show`、`new`、`edit` などをそれぞれ同じ Controller クラス上の別々のメソッドとして定義できます。

**移行前（HTTP メソッドディスパッチ）:**

```common-lisp
;; ルート定義
(setf clails/environment:*routing-tables*
  '((:path "/todos"     :controller "your-app/controllers/todo-controller::<todo-controller>")
    (:path "/todos/:id" :controller "your-app/controllers/todo-controller::<todo-controller>")))

;; Controller
(defclass <todo-controller> (<web-controller>) ())

(defmethod do-get ((controller <todo-controller>))
  (set-view controller "todos/index.html" `(:todos ,(find-all))))

(defmethod do-put ((controller <todo-controller>))
  (mark-as-done (find-by-id (param controller "id")))
  (set-redirect controller "/todos"))
```

HTML の `<form>` は PUT をネイティブに送信できないため、上記の `do-put` には通常、`_method=PUT` の隠しフィールドを付けて POST することでしか到達できません。

**移行後（`:action` ベースのルーティング）:**

```common-lisp
;; ルート定義
(setf clails/environment:*routing-tables*
  '((:path "/todos"     :controller "your-app/controllers/todo-controller::<todo-controller>"
     :action "index" :method :get)
    (:path "/todos/:id" :controller "your-app/controllers/todo-controller::<todo-controller>"
     :action "update" :method :put)))

;; Controller
(defclass <todo-controller> (<web-controller>) ())

(defmethod index ((controller <todo-controller>))
  (set-view controller "todos/index.html" `(:todos ,(find-all))))

(defmethod update ((controller <todo-controller>))
  (mark-as-done (find-by-id (param controller "id")))
  (set-redirect controller "/todos"))
```

（`resources` 関数は、CRUD の一式についてまさにこの形のルートテーブルを生成します。詳細は後述します。）

**プレーンな HTML フォームからの PUT/DELETE 移行時の注意点:** `:action` ベースのルートマッチングはリクエストの*実際の* HTTP メソッドを使い、`_method` は一切参照しません。そのため、`_method=PUT`/`_method=DELETE` を使って従来の `do-put`/`do-delete` に到達していたフォームは、同じ方法では `:method :put`/`:method :delete` のアクションルートに到達できません — リクエストは実際には POST のまま届き、そのパスに対して `:post` にマッチするルートが存在しないためです。このようなフォームを移行するには、実際に PUT/DELETE を送信する（例えば `fetch`/`XMLHttpRequest`、または JavaScript で拡張したフォーム送信を使う）か、スクリプトなしの素の HTML フォームをサポートし続ける必要がある場合は、そのパス用に `:method :post` のアクションを残しておいてください。

**起動時の警告:** `initialize-routing-tables` は、ルーティングテーブルのコンパイル1回につき1回だけ（たとえばアプリケーション起動時に1回、HTTP リクエストごとではありません）、`:action` が指定されておらず HTTP メソッドディスパッチにフォールバックするルートを一覧にした警告を1つ出力します。まだ移行が必要なルートを見つける助けになります。

HTTP メソッドディスパッチ機構の削除時期は現時点では決まっていません。将来のリリースで明示的に削除がアナウンスされるまでは動作し続けます。

### `resources` 関数による RESTful ルーティング

`resources` 関数を使うと、RESTful な7つの標準ルートを1行で定義できます。

```common-lisp
(setf clails/environment:*routing-tables*
  `(,@(resources "todos" "your-app/controllers/todo-controller::<todo-controller>")
    ,@(resources "blogs" "your-app/controllers/blog-controller::<blog-controller>")))
```

`(resources "todos" "controller")` は以下の7ルートを生成します。

| HTTP メソッド | パス | アクション | 用途 |
|---|---|---|---|
| GET | /todos | index | 一覧表示 |
| GET | /todos/new | new | 作成フォーム表示 |
| POST | /todos | create | 作成実行 |
| GET | /todos/:id | show | 詳細表示 |
| GET | /todos/:id/edit | edit | 編集フォーム表示 |
| PUT | /todos/:id | update | 更新実行 |
| DELETE | /todos/:id | destroy | 削除実行 |

`/todos/new` は `/todos/:id` より前に配置されるため、"new" が `:id` パラメータとしてマッチすることはありません。

#### `:only` オプション — 特定のアクションのみ生成

```common-lisp
;; index と show のみ生成
(resources "todos" "controller" :only '(:index :show))
```

#### `:except` オプション — 特定のアクションを除外

```common-lisp
;; destroy と update を除外
(resources "todos" "controller" :except '(:destroy :update))
```

#### `resources` を使った Controller の実装例

```common-lisp
(defclass <todo-controller> (<web-controller>)
  ())

(defmethod index ((controller <todo-controller>))
  (let ((todos (get-all-todos)))
    (set-view controller "todos/index.html" `(:todos ,todos))))

(defmethod show ((controller <todo-controller>))
  (let ((todo (find-todo (param controller "id"))))
    (set-view controller "todos/show.html" `(:todo ,todo))))

(defmethod new ((controller <todo-controller>))
  (set-view controller "todos/new.html"))

(defmethod create ((controller <todo-controller>))
  (let ((title (param controller "title")))
    (create-todo title)
    (set-redirect controller "/todos")))

(defmethod edit ((controller <todo-controller>))
  (let ((todo (find-todo (param controller "id"))))
    (set-view controller "todos/edit.html" `(:todo ,todo))))

(defmethod update ((controller <todo-controller>))
  (let ((id (param controller "id"))
        (title (param controller "title")))
    (update-todo id title)
    (set-redirect controller (format nil "/todos/~A" id))))

(defmethod destroy ((controller <todo-controller>))
  (destroy-todo (param controller "id"))
  (set-redirect controller "/todos"))
```

---

## 4. リクエストパラメータの取得

### `param` 関数

リクエストパラメータ（クエリパラメータ、POST データ、URL パラメータ）を取得します。

```common-lisp
(defmethod do-get ((controller <search-controller>))
  (let ((query (param controller "q"))
        (page (param controller "page")))
    ;; 検索処理...
    ))
```

### フォームデータの取得

POST リクエストのフォームデータも同様に取得できます。

```common-lisp
(defmethod do-post ((controller <users-controller>))
  (let ((name (param controller "name"))
        (email (param controller "email"))
        (age (parse-integer (param controller "age"))))
    ;; ユーザー作成処理...
    ))
```

---

## 5. View のレンダリング

### `set-view` メソッド

View テンプレートとデータを指定してレンダリングします。

```common-lisp
(defmethod do-get ((controller <users-controller>))
  (let ((users (execute-query
                 (query <user>
                        :as :user
                        :order-by ((:user :created-at :desc)))
                 '())))
    ;; View とデータを設定
    (set-view controller "users/index.html"
              `(:users ,users
                :title "ユーザー一覧"))))
```

### View ファイルのパス

View ファイルは `app/views/` ディレクトリからの相対パスで指定します。

```common-lisp
;; app/views/users/index.html を使用
(set-view controller "users/index.html" data)

;; app/views/admin/users/show.html を使用
(set-view controller "admin/users/show.html" data)
```

### View パッケージの自動解決

View パスから自動的にパッケージ名が解決されます。

- `"index.html"` → `:your-app/views/package`
- `"users/show.html"` → `:your-app/views/users/package`
- `"admin/users/list.html"` → `:your-app/views/admin/users/package`

---

## 6. リダイレクト

### `set-redirect` メソッド

指定したパスにリダイレクトします。

```common-lisp
(defmethod do-post ((controller <users-controller>))
  (let ((user (create-user "Taro" "taro@example.com")))
    ;; ユーザー作成後、一覧ページにリダイレクト
    (set-redirect controller "/users")))
```

### 絶対 URL へのリダイレクト

絶対 URL（http:// または https:// で始まる）も指定できます。

```common-lisp
(defmethod do-get ((controller <external-controller>))
  ;; 外部サイトにリダイレクト
  (set-redirect controller "https://example.com/"))
```

### リダイレクトの動作

- HTTP ステータスコードは 302（Found）
- Location ヘッダーが自動的に設定されます
- 相対パスの場合は、リクエストのスキーム、ホスト、ポートから完全な URL が構築されます

---

## 7. REST API の実装

### REST Controller の定義

```common-lisp
(defclass <api-users-controller> (<rest-controller>)
  ()
  (:documentation "REST API for user resources"))
```

### JSON レスポンスの返却

`set-response` メソッドで連想リストを設定します。

```common-lisp
(defmethod do-get ((controller <api-users-controller>))
  (let ((users (get-all-users)))
    (set-response controller
                  `((:status . "success")
                    (:data . ,(mapcar #'user-to-alist users))))))

(defun user-to-alist (user)
  `((:id . ,(ref user :id))
    (:name . ,(ref user :name))
    (:email . ,(ref user :email))))
```

### REST API のエラーハンドリング

エラー時は適切なステータスコードとメッセージを返します。

```common-lisp
(defmethod do-get ((controller <api-user-controller>))
  (let* ((id (param controller "id"))
         (user (find-user-by-id id)))
    (if user
        (set-response controller
                      `((:status . "success")
                        (:data . ,(user-to-alist user))))
        (progn
          (setf (slot-value controller 'code) 404)
          (set-response controller
                        `((:status . "error")
                          (:message . "User not found")))))))
```

---

## 8. レスポンスのカスタマイズ

### HTTP ステータスコードの設定

```common-lisp
(defmethod do-post ((controller <users-controller>))
  (let ((user (create-user "Taro" "taro@example.com")))
    ;; 201 Created を設定
    (setf (slot-value controller 'code) 201)
    (set-response controller
                  `((:status . "success")
                    (:data . ,(user-to-alist user))))))
```

### HTTP ヘッダーの設定

```common-lisp
(defmethod do-get ((controller <download-controller>))
  (let ((file-content (read-file-content)))
    ;; カスタムヘッダーを設定
    (setf (slot-value controller 'header)
          `(:content-type "application/octet-stream"
            :content-disposition "attachment; filename=\"data.csv\""))
    ;; レスポンスを設定
    ...))
```

### 主要な HTTP ステータスコード

- `200` - OK（デフォルト）
- `201` - Created（リソース作成成功）
- `204` - No Content（レスポンスボディなし）
- `302` - Found（リダイレクト）
- `400` - Bad Request（不正なリクエスト）
- `404` - Not Found（リソースが見つからない）
- `500` - Internal Server Error（サーバーエラー）

---

## 9. Controller のライフサイクル

### リクエスト処理の流れ

1. **ルーティング**: URL パスと HTTP メソッドから対応する Controller を検索
2. **インスタンス化**: Controller のインスタンスを作成
3. **パラメータ設定**: URL パラメータ、クエリパラメータ、POST データを設定
4. **アクション設定**: ルートに `:action` が指定されていれば、Controller の `action` スロットにセット
5. **メソッド呼び出し**: アクションが指定されていればアクションメソッドを、そうでなければ `do-get`、`do-post` などを呼び出し
6. **レスポンス生成**: View のレンダリング、またはレスポンスデータの返却

### Controller のスロット

Controller インスタンスには以下のスロットがあります。

#### `<base-controller>` のスロット

- `request` - HTTP リクエストオブジェクト
- `env` - 環境変数（lack 環境）
- `code` - HTTP ステータスコード（デフォルト: 200）
- `header` - HTTP レスポンスヘッダー（plist）
- `params` - リクエストパラメータ（ハッシュテーブル）
- `action` - アクション名（例: "index"、"show"）。ルートに `:action` が指定された場合にセットされる

#### `<web-controller>` の追加スロット

- `view` - View テンプレートのパス名
- `view-data` - View に渡すデータ
- `view-package` - View レンダリング用のパッケージ名

#### `<rest-controller>` の追加スロット

- `response` - レスポンスデータ（連想リスト）

---

## 10. 実践的な例

### CRUD 操作の実装

```common-lisp
(defclass <users-controller> (<web-controller>)
  ())

;; 一覧表示 (GET /users)
(defmethod do-get ((controller <users-controller>))
  (let ((users (execute-query
                 (query <user>
                        :as :user
                        :order-by ((:user :name)))
                 '())))
    (set-view controller "users/index.html"
              `(:users ,users
                :title "ユーザー一覧"))))

;; 作成 (POST /users)
(defmethod do-post ((controller <users-controller>))
  (let* ((name (param controller "name"))
         (email (param controller "email"))
         (user (make-record '<user> :name name :email email)))
    (if (save user)
        (set-redirect controller (format nil "/users/~A" (ref user :id)))
        (set-view controller "users/new.html"
                  `(:user ,user
                    :errors ,(get-errors user))))))

(defclass <user-controller> (<web-controller>)
  ())

;; 詳細表示 (GET /users/:id)
(defmethod do-get ((controller <user-controller>))
  (let* ((id (param controller "id"))
         (user (first (execute-query
                        (query <user>
                               :as :user
                               :where (:= (:user :id) :id))
                        `(:id ,id)))))
    (if user
        (set-view controller "users/show.html"
                  `(:user ,user))
        (error '404/not-found))))

;; 更新 (PUT /users/:id)
(defmethod do-put ((controller <user-controller>))
  (let* ((id (param controller "id"))
         (user (first (execute-query
                        (query <user>
                               :as :user
                               :where (:= (:user :id) :id))
                        `(:id ,id))))
         (name (param controller "name")))
    (when user
      (setf (ref user :name) name)
      (save user))
    (set-redirect controller (format nil "/users/~A" id))))

;; 削除 (DELETE /users/:id)
(defmethod do-delete ((controller <user-controller>))
  (let* ((id (param controller "id"))
         (user (first (execute-query
                        (query <user>
                               :as :user
                               :where (:= (:user :id) :id))
                        `(:id ,id)))))
    (when user
      (destroy user))
    (set-redirect controller "/users")))
```

### トランザクションを使った複雑な処理

```common-lisp
(defmethod do-post ((controller <order-controller>))
  (clails/model/transaction:with-transaction
    (let* ((user-id (param controller "user-id"))
           (product-id (param controller "product-id"))
           (quantity (parse-integer (param controller "quantity")))
           (product (find-product product-id)))
      
      ;; 在庫チェック
      (unless (>= (ref product :stock) quantity)
        (error "Not enough stock"))
      
      ;; 注文作成
      (let ((order (make-record '<order>
                                :user-id user-id
                                :product-id product-id
                                :quantity quantity
                                :total-price (* (ref product :price) quantity))))
        (save order)
        
        ;; 在庫更新
        (setf (ref product :stock) (- (ref product :stock) quantity))
        (save product)
        
        ;; 成功時のリダイレクト
        (set-redirect controller (format nil "/orders/~A" (ref order :id)))))))
```

### REST API の実装例

```common-lisp
(defclass <api-users-controller> (<rest-controller>)
  ())

;; 一覧取得 (GET /api/users)
(defmethod do-get ((controller <api-users-controller>))
  (let ((users (get-all-users)))
    (set-response controller
                  `((:status . "success")
                    (:count . ,(length users))
                    (:data . ,(mapcar #'user-to-json users))))))

;; 作成 (POST /api/users)
(defmethod do-post ((controller <api-users-controller>))
  (let* ((name (param controller "name"))
         (email (param controller "email"))
         (user (make-record '<user> :name name :email email)))
    (if (save user)
        (progn
          (setf (slot-value controller 'code) 201)
          (set-response controller
                        `((:status . "success")
                          (:data . ,(user-to-json user)))))
        (progn
          (setf (slot-value controller 'code) 400)
          (set-response controller
                        `((:status . "error")
                          (:errors . ,(get-errors-json user))))))))

(defun user-to-json (user)
  `((:id . ,(ref user :id))
    (:name . ,(ref user :name))
    (:email . ,(ref user :email))
    (:created-at . ,(format-datetime (ref user :created-at)))))
```

### ページネーション

```common-lisp
(defmethod do-get ((controller <users-controller>))
  (let* ((page (or (parse-integer (param controller "page") :junk-allowed t) 1))
         (per-page 20)
         (offset (* (1- page) per-page))
         (users (execute-query
                  (query <user>
                         :as :user
                         :order-by ((:user :created-at :desc))
                         :limit per-page
                         :offset offset)
                  '()))
         (total-count (count-users)))
    (set-view controller "users/index.html"
              `(:users ,users
                :page ,page
                :per-page ,per-page
                :total-count ,total-count
                :total-pages ,(ceiling total-count per-page)))))
```

---

## 11. エラーハンドリング

### 404 Not Found

デフォルトの実装では、未定義の HTTP メソッドは `404/not-found` エラーを発生させます。

```common-lisp
;; do-get を実装しない場合、自動的に 404 エラー
(defclass <my-controller> (<base-controller>)
  ())

;; カスタム 404 エラー
(defmethod do-get ((controller <my-controller>))
  (error '404/not-found :path (getf (env controller) :path-info)))
```

### エラーハンドリング Controller

エラーページを表示する専用の Controller を作成できます。

```common-lisp
(defclass <error-controller> (<web-controller>)
  ())

(defmethod do-get ((controller <error-controller>))
  (setf (slot-value controller 'code) 500)
  (set-view controller "errors/500.html"
            `(:message "Internal Server Error")))
```

---

## セッションと `current-user`

clails には、オプトイン方式の Cookie セッションと、その上に構築された
`current-user` 拡張ポイントも用意されています。コントローラからは
`(session controller)` と `(current-user controller)` で利用できます。
ログイン/ログアウトの完全な例を含む詳細は
**[セッション・認証ガイド](session_ja.md)** を参照してください。

---

## 12. ベストプラクティス

### Controller の責務

Controller は以下の責務のみを持つべきです。

1. **リクエストの検証**: パラメータの存在チェック、型チェック
2. **Model の呼び出し**: ビジネスロジックは Model に委譲
3. **レスポンスの構築**: View やレスポンスデータの設定

### ビジネスロジックの分離

複雑なビジネスロジックは Model や Service クラスに切り出します。

```common-lisp
;; 悪い例: Controller にビジネスロジックを書く
(defmethod do-post ((controller <order-controller>))
  (let ((product (find-product (param controller "product-id"))))
    ;; 複雑な計算や検証...
    ))

;; 良い例: Service クラスに切り出す
(defmethod do-post ((controller <order-controller>))
  (let ((order-service (make-instance '<order-service>)))
    (create-order order-service
                  :user-id (param controller "user-id")
                  :product-id (param controller "product-id")
                  :quantity (param controller "quantity"))))
```

### パラメータのバリデーション

パラメータは必ず検証します。

```common-lisp
(defmethod do-post ((controller <users-controller>))
  (let ((name (param controller "name"))
        (email (param controller "email")))
    ;; バリデーション
    (unless (and name email)
      (setf (slot-value controller 'code) 400)
      (set-response controller
                    `((:status . "error")
                      (:message . "Name and email are required")))
      (return-from do-post))
    
    ;; 処理続行...
    ))
```

### RESTful な設計

REST API は RESTful な設計原則に従います。

- `GET /users` - 一覧取得
- `GET /users/:id` - 詳細取得
- `POST /users` - 作成
- `PUT /users/:id` - 更新
- `DELETE /users/:id` - 削除

---

## まとめ

clails の Controller は以下の特徴を持ちます。

1. **シンプルな設計**: HTTP メソッドごとにメソッドを定義するだけ
2. **RESTful ルーティング**: `resources` 関数による Rails ライクなルート定義（`:only` / `:except` オプション対応）
3. **柔軟なルーティング**: URL パラメータの自動抽出とパターンマッチング
4. **View の統合**: `set-view` による簡単な View レンダリング
5. **REST API サポート**: `<rest-controller>` による JSON レスポンスの返却
6. **トランザクション対応**: Model と連携したトランザクション管理
7. **後方互換性**: 従来の `do-get` / `do-post` 方式と新しいアクション方式が共存可能。従来方式は非推奨であり、`:action` ベースのルーティングへの移行を推奨します（[「HTTP メソッドディスパッチから `:action` ベースのルーティングへの移行」](#http-メソッドディスパッチから-action-ベースのルーティングへの移行)を参照）

詳細な API リファレンスについては、各関数の docstring を参照してください。
