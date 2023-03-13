# git-contribution-visualizer

チーム開発を行っている Git のコントリビュートを可視化する。

## 利用方法

1. [Google スプレッドシート](https://docs.google.com/spreadsheets/d/14Z2yCX8wDWQYJPoX9CiObmQw-hy1Wlt4CvrxAiXZ1ww/edit#gid=0)をコピー、またはダウンロードしておく。
2. 以下コマンドを実行してインストール

    ```shell-session
    $ git clone git@github.com:satosystems/git-contribution-visualizer.git
    ...
    $ cd git-contribution-visualizer
    $ stack install
    ...
    $ ln -s "$(pwd)/shell/git-cvb" "$HOME/.local/bin/git-cvb"
    $ ln -s "$(pwd)/shell/git-cvc" "$HOME/.local/bin/git-cvc"
    $ export PATH="$HOME/.local/bin:$PATH"
    $
    ```

3. 以下コマンドの出力をスプレッドシートの Commits シートにペースト

    ```shell-session
    $ git cvc | pbcopy # macOS の場合
    ...
    $
    ```

4. 以下コマンドの出力をスプレッドシートの Blames シートにペースト

    ```shell-session
    $ git cvb | pbcopy # macOS の場合
    ...
    $
    ```

5. Author mapping シートを任意で修正し、名寄を行う

## 可視化

Charts シートに可視化される。

![git-contribution-visualizer_charts](https://user-images.githubusercontent.com/1842469/222305124-14d4b8d1-11c8-4d21-88ee-0272af6c508b.png)
