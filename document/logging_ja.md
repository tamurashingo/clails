# clails Logging ガイド

## 概要

clails の Logging は柔軟なロギングシステムを提供します。
アプリケーション全体のログ出力を統一的に管理し、パッケージ階層に基づいた自動的なロガーの選択、複数の出力先への同時出力、フォーマットのカスタマイズなどをサポートします。

## 基本概念

- **Logger**: ログメッセージを受け取り、設定されたログレベルに基づいて出力するかどうかを判断します
- **Appender**: ログの出力先を定義します（コンソール、ファイル、など）
- **Formatter**: ログメッセージの出力フォーマットを定義します（テキスト、JSON）
- **Log Level**: ログの重要度を表します（TRACE < DEBUG < INFO < WARN < ERROR < FATAL）
- **Logger 階層**: パッケージ階層に基づいて Logger を自動的に選択します

---

## 1. ログレベル

clails は以下のログレベルをサポートしています。

| レベル | 説明 | 用途 |
|--------|------|------|
| `:trace` | 最も詳細な情報 | デバッグ時の詳細なトレース情報 |
| `:debug` | デバッグ情報 | 開発時のデバッグ情報 |
| `:info` | 一般的な情報 | アプリケーションの通常動作の記録 |
| `:warn` | 警告 | 潜在的な問題の警告 |
| `:error` | エラー | エラーが発生したが、処理は継続可能 |
| `:fatal` | 致命的なエラー | アプリケーションの実行を継続できない重大なエラー |
| `:none` | ログ出力なし | すべてのログを抑制 |

---

## 2. Logger の設定

### 2.1. Logger の登録

`register-logger` 関数を使用して Logger を登録します。

```common-lisp
(use-package :clails/logger)

;; コンソールに出力する Logger を登録
(register-logger :root
                 :level :info
                 :appender (make-console-appender
                            :formatter (make-instance '<text-formatter>)))

;; ファイルに出力する Logger を登録
(register-logger :sql
                 :level :debug
                 :appender (make-file-appender
                            :filepath "/var/log/myapp/sql.log"
                            :formatter (make-instance '<json-formatter>)))
```

### 2.2. 階層的な Logger

clails は パッケージ階層に基づいて Logger を自動的に選択します。

```common-lisp
;; ルート Logger（すべてのログのデフォルト）
(register-logger :root
                 :level :info
                 :appender (make-console-appender
                            :formatter (make-instance '<text-formatter>)))

;; CLAILS パッケージ用の Logger
(register-logger :clails
                 :level :debug
                 :appender (make-console-appender
                            :formatter (make-instance '<text-formatter>)))

;; CLAILS/MODEL パッケージ用の Logger
(register-logger :clails/model
                 :level :trace
                 :appender (make-file-appender
                            :filepath "/var/log/myapp/model.log"
                            :formatter (make-instance '<json-formatter>)))
```

Logger が見つからない場合、親 Logger を自動的に検索します。

- `:clails/model/query` → `:clails/model` → `:clails` → `:root`

### 2.3. Logger の取得

`get-logger` 関数を使用して Logger を取得します。

```common-lisp
;; 名前で Logger を取得
(get-logger :root)       ; => ルート Logger
(get-logger :clails)     ; => CLAILS Logger

;; パッケージ名から Logger を取得（自動的に階層検索）
(get-logger :clails/model/query)  ; => :clails/model または :clails または :root
```

### 2.4. Logger の削除

`remove-logger` 関数を使用して Logger を削除します。

```common-lisp
(remove-logger :sql)
```

### 2.5. すべての Logger をクリア

`clear-loggers` 関数を使用してすべての Logger を削除します。

```common-lisp
(clear-loggers)
```

---

## 3. Appender の設定

### 3.1. コンソール Appender

標準出力にログを出力します。

```common-lisp
(make-console-appender
 :formatter (make-instance '<text-formatter>))
```

**特徴:**
- スレッドセーフ: `*standard-output*` の動的な値を使用します
- 複数スレッドから同時にログ出力しても安全です

### 3.2. ファイル Appender

ファイルにログを出力します。

```common-lisp
(make-file-appender
 :filepath "/var/log/myapp/app.log"
 :formatter (make-instance '<text-formatter>))
```

**特徴:**
- ファイルが存在しない場合は自動的に作成されます
- 既存のファイルには追記（append）されます
- ファイルストリームは自動的に管理されます

### 3.3. Appender のクローズ

ファイル Appender を使用している場合は、アプリケーション終了時に明示的にクローズすることを推奨します。

```common-lisp
(defvar *file-appender* (make-file-appender
                         :filepath "/var/log/myapp/app.log"
                         :formatter (make-instance '<text-formatter>)))

;; アプリケーション終了時
(close-appender *file-appender*)
```

---

## 4. Formatter の設定

### 4.1. テキスト Formatter

人間が読みやすいテキスト形式でログを出力します。

```common-lisp
(make-instance '<text-formatter>)
```

**出力例:**
```
[2024-01-15T10:30:45.123456+09:00] INFO: User logged in user-id=123 ip-address="192.168.1.1"
[2024-01-15T10:30:46.234567+09:00] ERROR: Database connection failed error="Connection timeout"
```

### 4.2. JSON Formatter

JSON 形式でログを出力します。機械処理や外部システムとの連携に便利です。

```common-lisp
(make-instance '<json-formatter>)
```

**出力例:**
```json
{"timestamp":"2024-01-15T10:30:45.123456+09:00","level":"info","message":"User logged in","user-id":123,"ip-address":"192.168.1.1"}
{"timestamp":"2024-01-15T10:30:46.234567+09:00","level":"error","message":"Database connection failed","error":"Connection timeout"}
```

---

## 5. ログの出力

### 5.1. パッケージベースのログ出力

現在のパッケージに基づいて自動的に Logger を選択してログを出力します。

```common-lisp
(in-package #:myapp/model)

;; 各ログレベルでログを出力
(log-package.trace "Entering function" :function "find-user" :args '(123))
(log-package.debug "Query executed" :query "SELECT * FROM users WHERE id = ?" :params '(123))
(log-package.info "User found" :user-id 123 :user-name "Alice")
(log-package.warn "Slow query detected" :query-time 5.2 :threshold 3.0)
(log-package.error "Failed to save user" :error "Validation failed" :user-id 123)
(log-package.fatal "Database connection lost" :error "Connection timeout")
```

**Logger の選択:**
- 現在のパッケージが `#:myapp/model` の場合
- `:myapp/model` → `:myapp` → `:root` の順で Logger を検索します
- 最初に見つかった Logger を使用します

### 5.2. 名前を指定したログ出力

Logger 名を明示的に指定してログを出力します。

```common-lisp
(log-to :root :info "Application started")
(log-to :sql :debug "Query executed" :query "SELECT * FROM users")
(log-to :myapp/service :error "Service failed" :service "UserService")
```

### 5.3. 用途別のログ出力

特定の用途に特化したログ出力マクロを使用できます。

#### SQL ログ

データベースクエリの記録に使用します。

```common-lisp
(log.sql "SELECT * FROM users WHERE id = ?"
         :params '(123)
         :query-time 0.05
         :rows-affected 1)
```

- Logger 名: `:sql`
- ログレベル: `:debug`

#### Web アクセスログ

HTTP リクエスト/レスポンスの記録に使用します。

```common-lisp
(log.web-access "GET /users/123"
                :method "GET"
                :path "/users/123"
                :status 200
                :duration 0.123
                :ip-address "192.168.1.1")
```

- Logger 名: `:web-access`
- ログレベル: `:info`

#### 監査ログ

セキュリティ関連のイベントやユーザーアクションの記録に使用します。

```common-lisp
(log.audit "User login successful"
           :user-id 123
           :username "alice"
           :ip-address "192.168.1.1"
           :action "login")
```

- Logger 名: `:audit`
- ログレベル: `:info`

#### タスクログ

タスク実行のイベント記録に使用します。

```common-lisp
(log.task "Task started"
          :task-name "db:migrate"
          :namespace :db)

(log.task "Task completed"
          :task-name "cleanup"
          :namespace :system
          :duration 1.234
          :status :success)
```

- Logger 名: `:task`
- ログレベル: `:info`

---

## 6. コンテキストの追加

`with-log-context` マクロを使用して、ブロック内のすべてのログメッセージに共通のコンテキストを追加できます。

```common-lisp
(with-log-context (:request-id "req-12345" :user-id 123)
  (log-package.info "Processing request")
  (log-package.debug "Fetching user data")
  (log-package.info "Request completed"))

;; 出力例:
;; [2024-01-15T10:30:45.123456+09:00] INFO: Processing request request-id="req-12345" user-id=123
;; [2024-01-15T10:30:45.234567+09:00] DEBUG: Fetching user data request-id="req-12345" user-id=123
;; [2024-01-15T10:30:45.345678+09:00] INFO: Request completed request-id="req-12345" user-id=123
```

### ネストしたコンテキスト

コンテキストはネストして使用できます。

```common-lisp
(with-log-context (:request-id "req-12345")
  (log-package.info "Request started")
  
  (with-log-context (:operation "fetch-user" :user-id 123)
    (log-package.debug "Fetching user")
    (log-package.info "User fetched"))
  
  (log-package.info "Request completed"))

;; 出力例:
;; [2024-01-15T10:30:45.123456+09:00] INFO: Request started request-id="req-12345"
;; [2024-01-15T10:30:45.234567+09:00] DEBUG: Fetching user request-id="req-12345" operation="fetch-user" user-id=123
;; [2024-01-15T10:30:45.345678+09:00] INFO: User fetched request-id="req-12345" operation="fetch-user" user-id=123
;; [2024-01-15T10:30:45.456789+09:00] INFO: Request completed request-id="req-12345"
```

---

## 7. ログレベルの動的変更

アプリケーション実行中にログレベルを動的に変更できます。

### 7.1. ログレベルの変更

```common-lisp
;; Logger のログレベルを変更
(set-logger-level :root :debug)
(set-logger-level :clails/model :trace)
(set-logger-level :sql :info)
```

### 7.2. ログレベルの確認

```common-lisp
;; 現在のパッケージで指定したログレベルが有効かどうかを確認
(log-level-enabled-p :debug)  ; => T or NIL

;; 特定のパッケージで指定したログレベルが有効かどうかを確認
(log-level-enabled-p :debug :clails/model)  ; => T or NIL
(log-level-enabled-p :trace :sql)           ; => T or NIL
```

### 7.3. 条件付きログ出力

ログレベルが有効な場合のみ、重い処理を実行してログ出力できます。

```common-lisp
(when (log-level-enabled-p :debug)
  (let ((expensive-data (compute-expensive-debug-info)))
    (log-package.debug "Debug info" :data expensive-data)))
```

---

## 8. Appender の動的追加

実行時に Logger に Appender を追加できます。

```common-lisp
;; 既存の Logger に Appender を追加
(add-appender :root
              (make-file-appender
               :filepath "/var/log/myapp/debug.log"
               :formatter (make-instance '<text-formatter>)))

;; 複数の Appender を持つ Logger
(let ((logger (get-logger :root)))
  ;; この Logger は console と file の両方に出力する
  (log-to :root :info "This message goes to both console and file"))
```

---

## 9. 実践的な使用例

### 9.1. 基本的なセットアップ

```common-lisp
(defun setup-logging ()
  "アプリケーションのログ設定を初期化"
  
  ;; ルート Logger: コンソールと一般ログファイル
  (register-logger :root
                   :level :info
                   :appender (make-console-appender
                              :formatter (make-instance '<text-formatter>)))
  (add-appender :root
                (make-file-appender
                 :filepath "/var/log/myapp/app.log"
                 :formatter (make-instance '<text-formatter>)))
  
  ;; SQL Logger: SQL専用ログファイル（JSON形式）
  (register-logger :sql
                   :level :debug
                   :appender (make-file-appender
                              :filepath "/var/log/myapp/sql.log"
                              :formatter (make-instance '<json-formatter>)))
  
  ;; Web アクセスログ: アクセスログ専用ファイル
  (register-logger :web-access
                   :level :info
                   :appender (make-file-appender
                              :filepath "/var/log/myapp/access.log"
                              :formatter (make-instance '<text-formatter>)))
  
  ;; 監査ログ: 監査専用ファイル（JSON形式）
  (register-logger :audit
                   :level :info
                   :appender (make-file-appender
                              :filepath "/var/log/myapp/audit.log"
                              :formatter (make-instance '<json-formatter>)))
  
  ;; タスクログ: タスク実行専用ファイル
  (register-logger :task
                   :level :info
                   :appender (make-file-appender
                              :filepath "/var/log/myapp/task.log"
                              :formatter (make-instance '<text-formatter>))))

;; アプリケーション起動時
(setup-logging)
```

### 9.2. Controller での使用例

```common-lisp
(in-package #:myapp/controller)

(defun show-user (params)
  "ユーザー情報を表示"
  (let ((user-id (parse-integer (getf params :id))))
    (with-log-context (:action "show-user" :user-id user-id)
      (log-package.info "Fetching user")
      
      (handler-case
          (let ((user (find-user-by-id user-id)))
            (if user
                (progn
                  (log-package.info "User found")
                  (render-user user))
                (progn
                  (log-package.warn "User not found")
                  (render-404))))
        (error (e)
          (log-package.error "Failed to fetch user" :error (princ-to-string e))
          (render-500))))))
```

### 9.3. Model での使用例

```common-lisp
(in-package #:myapp/model)

(defmethod save ((user <user>))
  "ユーザーを保存"
  (log-package.debug "Saving user" :user-id (ref user :id))
  
  (when (log-level-enabled-p :trace)
    (log-package.trace "User data" :data (show-model-data user)))
  
  (handler-case
      (progn
        (validate user)
        (if (has-error-p user)
            (progn
              (log-package.warn "Validation failed"
                               :errors (get-all-errors user))
              nil)
            (progn
              (with-transaction
                (if (ref user :id)
                    (update1 user)
                    (insert1 user)))
              (log-package.info "User saved successfully"
                               :user-id (ref user :id))
              user)))
    (error (e)
      (log-package.error "Failed to save user"
                        :error (princ-to-string e)
                        :user-id (ref user :id))
      (error e))))
```

### 9.4. 環境別の設定

```common-lisp
(defun setup-logging-for-environment ()
  "環境に応じたログ設定"
  (ecase (get-environment)
    (:development
     ;; 開発環境: すべて DEBUG レベル、コンソール出力
     (register-logger :root
                      :level :debug
                      :appender (make-console-appender
                                 :formatter (make-instance '<text-formatter>))))
    
    (:test
     ;; テスト環境: すべて INFO レベル、ファイル出力
     (register-logger :root
                      :level :info
                      :appender (make-file-appender
                                 :filepath "/tmp/test.log"
                                 :formatter (make-instance '<text-formatter>))))
    
    (:production
     ;; 本番環境: INFO レベル、JSON形式でファイル出力
     (register-logger :root
                      :level :info
                      :appender (make-file-appender
                                 :filepath "/var/log/myapp/app.log"
                                 :formatter (make-instance '<json-formatter>)))
     
     ;; エラーログは別ファイルに出力
     (register-logger :root
                      :level :error
                      :appender (make-file-appender
                                 :filepath "/var/log/myapp/error.log"
                                 :formatter (make-instance '<json-formatter>))))))
```

---

## 10. ベストプラクティス

### 10.1. パッケージベースのログ出力を優先

```common-lisp
;; 推奨: パッケージベースのログ出力
(log-package.info "User created" :user-id 123)

;; 非推奨: Logger 名を明示的に指定（必要な場合を除く）
(log-to :myapp/controller :info "User created" :user-id 123)
```

### 10.2. 適切なログレベルの使用

```common-lisp
;; TRACE: 詳細なデバッグ情報（通常は無効）
(log-package.trace "Function arguments" :args args)

;; DEBUG: 開発時のデバッグ情報
(log-package.debug "Query result" :count (length results))

;; INFO: 通常の動作記録
(log-package.info "User logged in" :user-id 123)

;; WARN: 潜在的な問題
(log-package.warn "Slow query detected" :query-time 5.2)

;; ERROR: エラーが発生したが継続可能
(log-package.error "Failed to send email" :error error-message)

;; FATAL: 致命的なエラー
(log-package.fatal "Database connection lost" :error error-message)
```

### 10.3. 構造化されたコンテキストの使用

```common-lisp
;; 推奨: キーワード引数で構造化
(log-package.info "User action"
                  :user-id 123
                  :action "update"
                  :resource "profile")

;; 非推奨: メッセージに情報を埋め込む
(log-package.info (format nil "User ~A performed ~A on ~A" 123 "update" "profile"))
```

### 10.4. 重い処理はログレベルチェック後に実行

```common-lisp
;; 推奨: ログレベルチェック
(when (log-level-enabled-p :debug)
  (let ((debug-info (expensive-debug-computation)))
    (log-package.debug "Debug information" :info debug-info)))

;; 非推奨: 常に重い処理を実行
(log-package.debug "Debug information"
                   :info (expensive-debug-computation))
```

### 10.5. コンテキストの活用

```common-lisp
;; 推奨: with-log-context でリクエストIDを共有
(with-log-context (:request-id request-id)
  (process-request)
  (save-result)
  (send-response))

;; 非推奨: すべてのログに手動で追加
(log-package.info "Processing" :request-id request-id)
(log-package.info "Saved" :request-id request-id)
(log-package.info "Sent" :request-id request-id)
```

---

## 11. トラブルシューティング

### 11.1. ログが出力されない

**原因1: Logger が登録されていない**

```common-lisp
;; 確認: Logger が登録されているか
(get-logger :root)  ; => NIL の場合は未登録

;; 対処: Logger を登録
(register-logger :root
                 :level :info
                 :appender (make-console-appender
                            :formatter (make-instance '<text-formatter>)))
```

**原因2: ログレベルが適切でない**

```common-lisp
;; 確認: ログレベルが有効か
(log-level-enabled-p :debug)  ; => NIL の場合は無効

;; 対処: ログレベルを変更
(set-logger-level :root :debug)
```

**原因3: 階層が一致しない**

```common-lisp
;; :myapp/controller パッケージでログ出力
(in-package #:myapp/controller)
(log-package.info "Test")  ; => 出力されない

;; 対処: ルート Logger を登録
(register-logger :root
                 :level :info
                 :appender (make-console-appender
                            :formatter (make-instance '<text-formatter>)))

;; または: パッケージに対応する Logger を登録
(register-logger :myapp
                 :level :info
                 :appender (make-console-appender
                            :formatter (make-instance '<text-formatter>)))
```

### 11.2. ファイルに書き込まれない

**原因: ファイルパスが不正**

```common-lisp
;; 対処: ディレクトリが存在することを確認
(ensure-directories-exist "/var/log/myapp/")

(register-logger :root
                 :level :info
                 :appender (make-file-appender
                            :filepath "/var/log/myapp/app.log"
                            :formatter (make-instance '<text-formatter>)))
```

---

## まとめ

clails の Logging システムは以下の特徴を持ちます。

1. **階層的な Logger**: パッケージ階層に基づいて自動的に Logger を選択
2. **柔軟な出力先**: コンソール、ファイルなど、複数の出力先をサポート
3. **カスタマイズ可能なフォーマット**: テキスト、JSON など、用途に応じたフォーマット
4. **動的な設定変更**: 実行時にログレベルや Appender を変更可能
5. **コンテキスト管理**: `with-log-context` による構造化されたログ出力
6. **用途別のログ出力**: SQL、Web アクセス、監査、タスクなど、用途に応じたログ出力マクロ
7. **スレッドセーフ**: 複数スレッドから安全にログ出力可能

詳細な API リファレンスについては、各関数の docstring を参照してください。
