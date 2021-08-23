<p align="center">
  <a href="https://github.com/pi-etro/func-invaders">
    <img src="https://raw.githubusercontent.com/pi-etro/func-invaders/main/img/func_invaders.svg" height="180">
  </a>
</p>
<p align="center">
    <a href="https://www.haskell.org/" alt="Made with Haskell">
        <img src="https://img.shields.io/badge/Made%20with-Haskell-5e5086" /></a>
    <a href="https://www.gnu.org/licenses/gpl-3.0.html" alt="GPLv3">
        <img src="https://img.shields.io/badge/License-GPLv3-CB0000.svg" /></a>
</p>

<br>

<div align="center">
    <a href="https://pi-etro.gitlab.io/func-invaders-web/">Play online now !</a>
</div>

## Controls

To **move** the cannon use `A`/`D` or ⬅️/➡️ keys and to **fire** use `space` or `left click`.

<p align="center">
  <img width="350" src="https://raw.githubusercontent.com/pi-etro/func-invaders/main/img/gameplay.gif">
</p>

## Web version

The [web version ](https://gitlab.com/pi-etro/func-invaders-web) uses the [shine](https://hackage.haskell.org/package/shine) library and can be played at [https://pi-etro.gitlab.io/func-invaders-web/](https://pi-etro.gitlab.io/func-invaders-web/).

## Build

To build use:

```bash
stack build
```
This project uses the [gloss](https://hackage.haskell.org/package/gloss) library, so it might be necessary to install some dependencies before stack builds the project successfully. Once the project is built, run the game with:

```bash
stack run
```

## About

Func Invaders is a Space Invaders clone implemented with the Haskell functional programming language. This project was developed by me for the Programming Paradigms course at UFABC.

Have fun!

## License
[GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.html)
