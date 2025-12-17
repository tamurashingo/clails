# clails タスクガイド

## 概要

このガイドでは、clailsのタスクシステムについて説明します。
タスクシステムは、データベース操作、データのシード、クリーンアップなど、アプリケーション固有の処理を定義して実行するための機能を提供します。

## 基本概念

- タスクは`deftask`マクロを使用して定義
- 関連するタスクを`defnamespace`マクロでグループ化可能
- タスクは`clails task`コマンドで実行
- タスクには依存関係を定義でき、実行時に自動的に依存タスクが先に実行される
- タスクファイルは`lib/tasks/`ディレクトリに配置
- カスタムタスクはアプリケーション起動時に自動的に読み込まれる

---

## 1. タスクプロジェクトの構造

clailsプロジェクトでは、タスクファイルは以下のように配置されます：

```
your-app/
├── app/
│   ├── controllers/
│   ├── models/
│   └── views/
├── lib/
│   └── tasks/           # カスタムタスクディレクトリ
│       ├── cleanup.lisp  # サンプルタスク
│       └── seed.lisp     # データのシード処理など
├── your-app.asd
└── your-app-test.asd
```

### タスクファイルの命名規則

- タスクファイルは`.lisp`拡張子を使用
- ファイル名は内容を表す分かりやすい名前をつける
- `lib/tasks/`配下の任意のディレクトリに配置可能（サブディレクトリも可）

---

## 2. タスクの定義

### deftaskを使用した基本的なタスク定義

`deftask`マクロを使用してタスクを定義します：

```common-lisp
(defpackage #:your-app/tasks/cleanup
  (:use #:cl)
  (:import-from #:clails/task/core
                #:deftask)
  (:import-from #:clails/logger/core
                #:log.task))

(in-package #:your-app/tasks/cleanup)

(deftask :cleanup
  :description "Clean up temporary files"
  :function (lambda ()
              (log.task "Task started" :task-name "cleanup")
              (format t "Cleaning up temporary files~%")
              ;; クリーンアップ処理をここに記述
              (log.task "Task completed" :task-name "cleanup")))
```

### 名前空間を使用したタスク定義

関連するタスクをグループ化するために`defnamespace`マクロを使用できます：

```common-lisp
(defpackage #:your-app/tasks/db-tasks
  (:use #:cl)
  (:import-from #:clails/task/core
                #:deftask
                #:defnamespace)
  (:import-from #:clails/logger/core
                #:log.task))

(in-package #:your-app/tasks/db-tasks)

(defnamespace :db
  (deftask :seed
    :description "Load seed data into database"
    :function (lambda ()
                (log.task "Task started" :task-name "db:seed")
                (format t "Loading seed data~%")
                ;; シード処理をここに記述
                (log.task "Task completed" :task-name "db:seed")))

  (deftask :reset
    :description "Reset database"
    :function (lambda ()
                (log.task "Task started" :task-name "db:reset")
                (format t "Resetting database~%")
                ;; リセット処理をここに記述
                (log.task "Task completed" :task-name "db:reset"))))
```

### タスクの依存関係

タスクには依存関係を定義できます。依存タスクは自動的に先に実行されます：

```common-lisp
(deftask :setup
  :description "Setup application"
  :function (lambda ()
              (format t "Setting up application~%")))

(deftask :deploy
  :description "Deploy application"
  :depends-on (:setup)
  :function (lambda ()
              (format t "Deploying application~%")))

;; 名前空間付きタスクへの依存
(deftask :full-deploy
  :description "Full deployment with database setup"
  :depends-on ((:db :migrate) (:db :seed) :deploy)
  :function (lambda ()
              (format t "Full deployment completed~%")))
```

### 引数を受け取るタスク

タスクは引数を受け取ることができます：

```common-lisp
(deftask :import-data
  :description "Import data from file"
  :args (&key (file "data.csv") (verbose nil))
  :function (lambda (&key (file "data.csv") (verbose nil))
              (format t "Importing data from: ~A~%" file)
              (when verbose
                (format t "Verbose mode enabled~%"))
              ;; データインポート処理をここに記述
              ))
```

---

## 3. タスクの生成

### `clails generate:task` - タスクファイルを生成

新しいタスクファイルを生成します。

#### 書式

```bash
clails generate:task TASK_NAME [OPTIONS]
```

#### オプション

| オプション | 短縮形 | 説明 |
|-----------|--------|------|
| `--namespace NS` | `-ns NS` | タスクの名前空間を指定 |

#### 使用例

```bash
# 単純なタスクを生成
clails generate:task cleanup

# 名前空間付きタスクを生成
clails generate:task seed -ns db
clails generate:task import --namespace data
```

#### 生成されるファイル

```
# 単純なタスク
lib/tasks/cleanup.lisp

# 名前空間付きタスク
lib/tasks/db/seed.lisp
lib/tasks/data/import.lisp
```

---

## 4. タスクの実行

### タスクの実行

`clails task`コマンドでタスクを実行します：

```bash
# 単純なタスクを実行
clails task cleanup

# 名前空間付きタスクを実行
clails task db:seed
clails task db:migrate
```

### タスク一覧の表示

利用可能なタスクを確認できます：

```bash
# すべてのタスクを表示
clails task --list

# 特定の名前空間のタスクを表示
clails task --list db
```

出力例：

```
Available tasks:

Global tasks:
  cleanup              Clean up temporary files

db:
  db:seed              Load seed data into database
  db:reset             Reset database
  db:migrate           Run database migrations
```

### タスクの詳細情報を表示

タスクの詳細情報を確認できます：

```bash
# タスクの詳細を表示
clails task --info cleanup
clails task --info db:seed
```

出力例：

```
Task: db:seed
Description: Load seed data into database
Dependencies: (:create :migrate)
```

---

## 5. タスクシステムの実装詳細

### タスクレジストリ

タスクは`clails/task/registry`パッケージで管理されています：

- `register-task`: タスクを登録
- `find-task`: タスクを検索
- `list-tasks`: タスク一覧を取得
- `list-namespaces`: 名前空間一覧を取得
- `load-custom-tasks`: カスタムタスクファイルを読み込み

### タスクランナー

タスクの実行は`clails/task/runner`パッケージで処理されます：

- 依存関係の自動解決
- タスクの冪等性（同じタスクは1回の実行で1度だけ実行される）
- エラーハンドリングとロギング

### タスク情報クラス

各タスクは`<task-info>`クラスで表現されます：

- `name`: タスク名
- `namespace`: 名前空間（オプション）
- `description`: 説明文
- `depends-on`: 依存タスクのリスト
- `args`: 引数リスト
- `function`: 実行する関数

---

## 6. 実践例

### データベースのシードタスク

```common-lisp
(defpackage #:your-app/tasks/db-seed
  (:use #:cl)
  (:import-from #:clails/task/core
                #:deftask
                #:defnamespace)
  (:import-from #:clails/logger/core
                #:log.task)
  (:import-from #:your-app/models/user
                #:<user>)
  (:import-from #:clails/model/core
                #:make-record
                #:save))

(in-package #:your-app/tasks/db-seed)

(defnamespace :db
  (deftask :seed
    :description "Load initial user data"
    :depends-on ((:db :migrate))
    :function (lambda ()
                (log.task "Task started" :task-name "db:seed")
                
                ;; ユーザーデータの作成
                (let ((users '(("Alice" "alice@example.com")
                             ("Bob" "bob@example.com")
                             ("Charlie" "charlie@example.com"))))
                  (dolist (user-data users)
                    (let ((user (make-record '<user>
                                            :name (first user-data)
                                            :email (second user-data))))
                      (save user)
                      (format t "Created user: ~A~%" (first user-data)))))
                
                (log.task "Task completed" :task-name "db:seed"))))
```

実行：

```bash
clails task db:seed
```

### ログファイルのクリーンアップタスク

```common-lisp
(defpackage #:your-app/tasks/log-cleanup
  (:use #:cl)
  (:import-from #:clails/task/core
                #:deftask)
  (:import-from #:clails/logger/core
                #:log.task)
  (:import-from #:uiop
                #:delete-file-if-exists))

(in-package #:your-app/tasks/log-cleanup)

(deftask :log-cleanup
  :description "Remove old log files"
  :args (&key (days 30))
  :function (lambda (&key (days 30))
              (log.task "Task started" 
                       :task-name "log-cleanup"
                       :days days)
              
              (format t "Removing log files older than ~A days~%" days)
              
              ;; ログファイルの削除処理
              (let ((log-dir (merge-pathnames "logs/" (uiop:getcwd)))
                    (cutoff-time (- (get-universal-time)
                                   (* days 24 60 60))))
                (dolist (file (uiop:directory-files log-dir "*.log"))
                  (when (< (file-write-date file) cutoff-time)
                    (delete-file-if-exists file)
                    (format t "Deleted: ~A~%" file))))
              
              (log.task "Task completed" :task-name "log-cleanup")))
```

実行：

```bash
# デフォルト（30日）
clails task log-cleanup

# カスタム日数（キーワード引数は現在未サポート、将来の拡張予定）
```

### レポート生成タスク

```common-lisp
(defpackage #:your-app/tasks/report
  (:use #:cl)
  (:import-from #:clails/task/core
                #:deftask)
  (:import-from #:clails/logger/core
                #:log.task))

(in-package #:your-app/tasks/report)

(deftask :generate-report
  :description "Generate monthly report"
  :depends-on (:backup-data)
  :function (lambda ()
              (log.task "Task started" :task-name "generate-report")
              
              (format t "Generating monthly report~%")
              
              ;; レポート生成処理
              (let ((report-file (format nil "report-~A.txt"
                                        (local-time:format-timestring
                                         nil
                                         (local-time:now)
                                         :format '(:year :month :day)))))
                (with-open-file (out report-file
                                    :direction :output
                                    :if-exists :supersede)
                  (format out "Monthly Report~%")
                  (format out "Generated: ~A~%"
                         (local-time:format-timestring nil (local-time:now))))
                (format t "Report saved to: ~A~%" report-file))
              
              (log.task "Task completed" :task-name "generate-report")))

(deftask :backup-data
  :description "Backup application data"
  :function (lambda ()
              (format t "Backing up data~%")
              ;; バックアップ処理
              ))
```

実行：

```bash
clails task generate-report
```

この場合、`generate-report`タスクは`backup-data`タスクに依存しているため、
`backup-data`が先に実行されます。

---

## 7. ベストプラクティス

### タスク設計のヒント

1. **単一責任の原則**: 各タスクは1つのことを行う
2. **冪等性**: 同じタスクを複数回実行しても安全であるように設計
3. **ロギング**: タスクの開始と完了をログに記録
4. **エラーハンドリング**: 適切なエラーメッセージを提供

### タスクの組織化

1. **名前空間の活用**: 関連タスクは名前空間でグループ化
2. **ファイル分割**: 大きなタスクセットは複数のファイルに分割
3. **依存関係の明示**: タスク間の依存関係を明確に定義

### パフォーマンスの考慮

1. **長時間実行タスク**: 進捗を表示
2. **データベースアクセス**: 適切なトランザクション管理
3. **バッチ処理**: 大量データは分割処理

---

## 8. トラブルシューティング

### タスクが見つからない

```
Error: Task not found: mytask
```

**解決策**:
- タスクファイルが`lib/tasks/`に配置されているか確認
- タスク名と名前空間が正しいか確認
- タスクが正しく`deftask`で定義されているか確認

### 依存タスクが見つからない

```
Dependency task not found: :setup
```

**解決策**:
- 依存タスクが定義されているか確認
- 依存タスクのファイルが読み込まれているか確認
- 名前空間付きタスクへの依存は`(:namespace :task-name)`形式で記述

### タスクファイルの読み込みエラー

```
Loading task file: /path/to/task.lisp ... failed: <error>
```

**解決策**:
- タスクファイルの構文エラーを確認
- 必要なパッケージが`:import-from`で宣言されているか確認
- Common Lispの構文が正しいか確認

---

## 9. まとめ

clailsのタスクシステムは、アプリケーション固有の処理を簡単に定義・実行できる強力な機能です：

- `deftask`マクロで簡単にタスクを定義
- `defnamespace`で関連タスクをグループ化
- 依存関係の自動解決
- `clails task`コマンドで簡単に実行
- ロギングとエラーハンドリングの組み込みサポート

タスクシステムを活用して、データベースのセットアップ、データのシード、クリーンアップ、レポート生成など、
様々なバッチ処理やメンテナンス作業を効率的に実行できます。
