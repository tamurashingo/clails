# clails Controller ガイド

## 概要

clails の Controller は Ruby on Rails の Controller を参考にした HTTP リクエストハンドラーです。
Controller は HTTP リクエストを受け取り、Model を使ってデータを処理し、View にデータを渡したり、JSON レスポンスを返したりします。

## 基本概念

- Controller は HTTP メソッド（GET、POST、PUT、DELETE）ごとに処理を定義します
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

1. **ルーティング**: URL パスから対応する Controller を検索
2. **インスタンス化**: Controller のインスタンスを作成
3. **パラメータ設定**: URL パラメータ、クエリパラメータ、POST データを設定
4. **メソッド呼び出し**: HTTP メソッドに応じて `do-get`、`do-post` などを呼び出し
5. **レスポンス生成**: View のレンダリング、またはレスポンスデータの返却

### Controller のスロット

Controller インスタンスには以下のスロットがあります。

#### `<base-controller>` のスロット

- `request` - HTTP リクエストオブジェクト
- `env` - 環境変数（lack 環境）
- `code` - HTTP ステータスコード（デフォルト: 200）
- `header` - HTTP レスポンスヘッダー（plist）
- `params` - リクエストパラメータ（ハッシュテーブル）

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
2. **柔軟なルーティング**: URL パラメータの自動抽出とパターンマッチング
3. **View の統合**: `set-view` による簡単な View レンダリング
4. **REST API サポート**: `<rest-controller>` による JSON レスポンスの返却
5. **トランザクション対応**: Model と連携したトランザクション管理

詳細な API リファレンスについては、各関数の docstring を参照してください。
