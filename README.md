# thermdrag

## Personal Starter / learning project

* Purescript
* Webpack 2
* Hot Code Reloading with Purs-loader 
* Halogen

Components:
Container - Websocket producer, consumer (from Halogens example)
Routing   - (from parsonsmatt)
Navbar    - plugged into the Routing, using the Bootstrap3 lib
Login     - plugged into Navbar, using Ajax to login and then sends the token as message to Routing, Container
Chat      - Chat using Ajax to get rooms etc. exchanges messages with Container to implement websocket
Sessions  - from parsonsmatts routing example just to illustrate that routing works

_The chat is using the chatqy Haskell Servant project as ajax, websocket server

_Im using Visual Studio Code, Purescript 0.11.4 built from source, and the font Hack

![alt tag](https://raw.githubusercontent.com/Rockfordal/thermdrag/halogen/images/chat1.png)

#### Installation Linux/Osx
* npm install
* bower install
* ./start

#### Sample Deployment
* ./build
* ./serve
