# haskell-sandbox

## 環境構築

M1 mac だとこれでうまく行った

### haskell 　を入れる

```shell
# haskell に関する tool を install
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# hlint を intall
cabal update
cabal install hlint
```

### vscode を入れる

[Download Visual Studio Code](https://code.visualstudio.com/download) から環境に合わせてインストーラをダウンロードして vscode を入れる。

入ったらこのリポジトリをクローンしたディレクトリを vscode で開いて拡張機能のタブで `@recommended` を入力する。すると haskell を書く上で必要な拡張機能が表示されるのですべて install する

### ライブラリインストール

xxx の部分を入れたいライブラリ名にして以下のコマンドを実行する。
vscode の window restart をして更新しているがもっといい方法があるはず

```shell
cabal install --lib xxx --package-env .
```

## 参考

- [Haskell の環境構築 2023](https://zenn.dev/mod_poppo/articles/haskell-setup-2023)
- [hlint](https://github.com/ndmitchell/hlint)
- [haskell-dev-env](https://github.com/vzarytovskii/haskell-dev-env)
