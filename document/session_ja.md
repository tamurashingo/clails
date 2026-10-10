# clails セッション・認証ガイド

## 概要

clails には、[`lack-middleware-session`](https://github.com/fukamachi/lack)
をベースにした **オプトイン方式** の Cookie セッションミドルウェアと、
そのセッションの上に構築された小さな `current-user` 拡張ポイントが
用意されています。

これは意図的にスコープを絞った機能です。

- **セッション機能** はフレームワークが提供する実質的な機能です。有効化
  するだけで、すべてのコントローラがリクエストごとの Cookie ベースの
  ハッシュテーブルを読み書きできるようになります。
- **`current-user`** は「配線」のみです。clails はパスワードのハッシュ化、
  ログイン/ログアウト用ルーティング、「ユーザー」モデルの規約など、認証の
  仕組みそのものは一切提供しません。「現在ログインしているユーザーが誰か」
  を保存するための、ドキュメント化されたセッションバックエンドの置き場所を
  提供するだけです。これにより、どのプロジェクトも同じような場当たり的な
  仕組みを個別に発明せずに済みます。ログインロジック(認証情報の検証、
  ユーザーレコードの検索など)自体はプロジェクト側で実装し、成功したら
  `(setf (current-user controller) ...)` を呼び出すだけです。

明示的に **対象外**(clails が提供せず、この機能の一部として計画もして
いないもの):

- CSRF 対策
- Remember-me(永続ログイン)トークン
- OAuth / OIDC 連携
- ロールベースの認可・パーミッションチェック
- 組み込みのパスワードハッシュ化ヘルパー

これらが必要な場合は、以下で説明する `session` / `current-user` の配線の
上に、プロジェクト側で実装するか、専用のライブラリを利用してください。

---

## 1. セッションミドルウェアの有効化

セッションミドルウェアは **デフォルトで無効** です。プロジェクト側で
`clails/middleware/session-middleware:*enable-session-middleware*` を `T`
に設定することでオプトインします。通常は `app/config/environment.lisp`
で行います。

```common-lisp
(in-package #:your-app/config/environment)

;; clails の Cookie セッションミドルウェアを有効化する
(setf clails/middleware/session-middleware:*enable-session-middleware* t)
```

これだけで有効になります。`*clails-middleware-stack*` 自体を変更する
必要はありません。clails は常にセッションミドルウェアをスタックに
組み込んでいますが、フラグが `NIL` のときは単なるパススルーとして動作し、
Cookie の発行もリクエストごとのハッシュテーブルの生成も一切行いません。

### ストアと Cookie の設定をカスタマイズする(任意)

デフォルトでは、セッションはインメモリストア
(`lack/middleware/session/store/memory:make-memory-store`)と標準的な
Cookie (`lack/middleware/session/state/cookie:make-cookie-state`)を使って
保持されます。どちらもサーバー起動前に上書きできます。

```common-lisp
;; Cookie 名や httponly フラグをカスタマイズする
(setf clails/middleware/session-middleware:*session-state*
      (lack/middleware/session/state/cookie:make-cookie-state
        :cookie-key "_your_app_session"
        :httponly t
        :secure t))

;; インメモリストアを lack-session-store-dbi / -redis などに差し替える
;; (setf clails/middleware/session-middleware:*session-store* ...)
```

インメモリストアはローカル開発や単一プロセスのデプロイには十分ですが、
複数プロセスで動かす場合や再起動をまたいでセッションを保持したい場合は、
`lack-session-store-dbi` や `lack-session-store-redis` のような共有ストア
が必要になります。

**重要:** これらの変数は、サーバー起動時にミドルウェアスタックが構築
される際(`clails:server`)に一度だけ読み取られます。サーバー起動前に
読み込まれる `app/config/environment.lisp` で設定してください。実行中に
値を変更しても、既に動作中の挙動には反映されません。

---

## 2. セッションデータの読み書き

有効化すると、すべてのコントローラインスタンスに `session` アクセサが
追加され、Cookie セッションに紐づいたリクエストスコープのハッシュテーブル
(`:test 'equal`)を返します。

```common-lisp
(defmethod do-get ((controller <my-controller>))
  ;; 読み取り
  (let ((visits (or (gethash "visits" (session controller)) 0)))
    ;; 書き込み
    (setf (gethash "visits" (session controller)) (1+ visits))
    (set-view controller "home/index.html"
              `(:visits ,(gethash "visits" (session controller))))))
```

セッションハッシュテーブルに格納したデータは、レスポンス生成後に
セッションストアへコミットされ、クライアントにはセッション Cookie が
発行(または維持)されるため、同じ訪問者からの次のリクエストでも同じ
ハッシュテーブルの内容にアクセスできます。

### セッションミドルウェアが無効な場合

`clails/middleware/session-middleware:*enable-session-middleware*` が
`NIL` のときに `(session controller)` を呼び出すと、ミドルウェアを
有効化するよう促すエラーが送出されます。これは意図的な挙動です。もし
空の使い捨てハッシュテーブルを黙って返してしまうと、データが保存され
ていないことに気づかないまま「消えてしまう」おそれがあるためです。

---

## 3. `current-user`:認証システムではなく拡張ポイント

`current-user` は `session` の上に直接構築された、薄いアクセサのペア
です。

```common-lisp
(current-user controller)              ; => 直近で保存した値、または NIL
(setf (current-user controller) user)  ; user をログイン中のユーザーとして保存
(setf (current-user controller) nil)   ; 「ログアウト」
```

内部的には、`current-user` は `session` が公開しているものと同じ
ハッシュテーブル内の専用キーを読み書きしているだけです。別の保存領域は
なく、ユーザーモデルの規約もなく、`lack-middleware-session` 自体が提供
する以上のセッション固定化対策もありません。`user` にはアプリケーションに
とって都合の良いものを何でも入れられます。フルのモデルインスタンスでも、
`:id`/`:name` の plist でも、リクエストごとに再検索するための ID の整数
値だけでも構いません。clails は中身を気にしません。

### 例示用のログイン/ログアウトの例

**この例は意図的に最小限にしてあり、本番運用に耐えるセキュリティレベル
では *ありません*。** `current-user` API の形を示すことが目的であり、
そのまま実際のログインシステムにコピー&ペーストするためのものでは
ありません。特に、パスワードのハッシュ化、タイミング攻撃耐性のある比較、
レート制限、CSRF 対策、セッション固定化対策(ログイン時のセッション ID
再生成など)は一切行っていません。おもちゃ以上のものを作る場合は、
これらすべてを自分で追加するか、専用のライブラリを使用してください。

```common-lisp
(defclass <session-controller> (<rest-controller>)
  ())

;; POST /login  { "email": "...", "password": "..." }
(defmethod do-post ((controller <session-controller>))
  (let* ((email (param controller "email"))
         (password (param controller "password"))
         ;; find-user-by-email / verify-password はプロジェクト側のコードです。
         ;; clails はユーザー検索やパスワード検証の機能を提供しません。
         (user (find-user-by-email email)))
    (if (and user (verify-password user password))
        (progn
          (setf (current-user controller) (list :id (ref user :id)
                                                  :email (ref user :email)))
          (set-response controller '((:status . "ok"))))
        (progn
          (setf (slot-value controller 'code) 401)
          (set-response controller '((:status . "error")
                                      (:message . "invalid credentials")))))))

;; DELETE /login (ログアウト)
(defmethod do-delete ((controller <session-controller>))
  (setf (current-user controller) nil)
  (set-response controller '((:status . "ok"))))
```

ログイン済みユーザーを要求するコントローラの例:

```common-lisp
(defclass <account-controller> (<web-controller>)
  ())

(defmethod do-get ((controller <account-controller>))
  (let ((user (current-user controller)))
    (if user
        (set-view controller "account/show.html" `(:user ,user))
        (set-redirect controller "/login"))))
```

このチェック(`(if (current-user controller) ... )`)は単なる素の Lisp
コードなので、ヘルパー関数や `:before` メソッドなど、プロジェクトの
規約に合わせて自由に切り出すことができます。clails は特定の認可パターンを
強制しません。

---

## 4. 完全な例

`app/config/environment.lisp`:

```common-lisp
(setf clails/middleware/session-middleware:*enable-session-middleware* t)
```

`app/controllers/greeting-controller.lisp`:

```common-lisp
(defclass <greeting-controller> (<rest-controller>)
  ())

;; GET /greeting/set?name=Alice
(defmethod index ((controller <greeting-controller>))
  (setf (gethash "name" (session controller)) (param controller "name"))
  (set-response controller '((:status . "ok"))))

;; GET /greeting/get
(defmethod show ((controller <greeting-controller>))
  (set-response controller
                `((:status . "ok")
                  (:name . ,(gethash "name" (session controller))))))
```

通常通り `index`/`show` にルートを割り当てれば([Controller Guide](controller_ja.md)
を参照)、リクエスト間で Cookie を保持するクライアント(ブラウザ、または
`curl -c cookies.txt -b cookies.txt`)であれば、最初のリクエストで設定した
名前が2回目のリクエストでも反映されて返ってきます。この動作は、本機能の
開発時に `curl -c cookies.txt ...` に続けて `curl -b cookies.txt ...` を
実際に動作中の clails サーバーに対して実行し、手動で end-to-end に確認済み
です。

---

## まとめ

1. セッションミドルウェアは **オプトイン** です。
   `app/config/environment.lisp` で `*enable-session-middleware*` を `T`
   に設定してください。
2. `(session controller)` は、リクエストスコープの Cookie ベースの
   ハッシュテーブルを返します。通常の `gethash` で読み書きできます。
3. `(current-user controller)` / `(setf (current-user controller) ...)`
   は、同じセッションを裏付けとした「誰がログインしているか」のための
   配線です。clails はログイン/ログアウト用のルーティング、パスワード
   処理、認可システムを一切提供しません。それらはプロジェクト側の責任
   です。
4. CSRF 対策、Remember-me トークン、OAuth/OIDC、ロールベースの認可、
   パスワードハッシュ化ヘルパーは明示的に **提供されません**。必要な
   場合は自分で実装するか、専用のライブラリを利用してください。
