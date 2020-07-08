const {webkit} = require('playwright');


(async () => {
  const appUrl = 'http://localhost:3000/'
  const browser = await webkit.launch({headless: true, acceptDownloads: true});

  const creatorContext = await browser.newContext();
  const creator = await creatorContext.newPage();

  const player1Context = await browser.newContext();
  const player1 = await player1Context.newPage();

  const player2Context = await browser.newContext();
  const player2 = await player2Context.newPage();

  // creator creates game
  await creator.setViewportSize({width: 411, height: 811});
  await creator.goto(appUrl);
  await creator.waitForSelector(ui('welcome'));

  await creator.screenshot({path: screenshotPath('creator', '01-welcome.png')});

  await creator.click(buttonWithText('Create game'));
  await creator.waitForSelector(ui('create-game'));

  await creator.screenshot({path: screenshotPath('creator', '02-1-create-game.png')});
  await creator.fill(labelledInput('Game name'), "Test game");
  await creator.fill(labelledInput('Screen name'), "Creator");
  await creator.screenshot({path: screenshotPath('creator', '02-2-create-game-with-input.png')});

  await creator.click(buttonWithText('Create game'));
  await creator.waitForSelector(textFragment('loading'));

  await creator.screenshot({path: screenshotPath('creator', '02-3-create-game-loading.png')});

  await creator.waitForSelector(ui('lobby'));

  await creator.screenshot({path: screenshotPath('creator', '03-1-lobby.png')});
  const gameCodeEl = await creator.$(uiHook("game-code"));
  const gameCode = await gameCodeEl.innerText();

  // player 1 join game
  await player1.goto(appUrl);
  await player1.waitForSelector(ui('welcome'));

  await player1.screenshot({path: screenshotPath('player1', '01-welcome.png')});

  await player1.click(buttonWithText('Join game'));
  await player1.waitForSelector(ui('join-game'));

  await player1.screenshot({path: screenshotPath('player1', '02-1-join-game.png')});
  await player1.fill(labelledInput('Game code'), gameCode);
  await player1.fill(labelledInput('Screen name'), "Player 1");
  await player1.screenshot({path: screenshotPath('player1', '02-2-join-game-with-input.png')});

  await player1.click(buttonWithText('Join game'));
  await player1.waitForSelector(ui('lobby'));

  await player1.screenshot({path: screenshotPath('player1', '03-1-lobby.png')});
  await creator.screenshot({path: screenshotPath('creator', '03-2-lobby.png')});

  // player 2 join game
  await player2.goto(appUrl);
  await player2.waitForSelector(ui('welcome'));

  await player2.screenshot({path: screenshotPath('player2', '01-welcome.png')});

  await player2.click(buttonWithText('Join game'));
  await player2.waitForSelector(ui('join-game'));

  await player2.screenshot({path: screenshotPath('player2', '02-1-join-game.png')});
  await player2.fill(labelledInput('Game code'), gameCode);
  await player2.fill(labelledInput('Screen name'), "Player 2");
  await player2.screenshot({path: screenshotPath('player2', '02-2-join-game-with-input.png')});

  await player2.click(buttonWithText('Join game'));
  await player2.waitForSelector(ui('lobby'));

  await player2.screenshot({path: screenshotPath('player2', '03-1-lobby.png')});
  await creator.screenshot({path: screenshotPath('creator', '03-3-lobby.png')});

  // creator start game
  await creator.click(buttonWithText('Start game'));
  await creator.waitForSelector(ui('place-disc'));

  await creator.screenshot({path: screenshotPath('creator', '04-1-initial-disc-screen.png')});

  await player1.waitForSelector(ui('place-disc'));
  await player1.screenshot({path: screenshotPath('player1', '04-1-initial-disc-screen.png')});

  await player2.waitForSelector(ui('place-disc'));
  await player2.screenshot({path: screenshotPath('player2', '04-1-initial-disc-screen.png')});

  // now in-game, initial discs round
  await creator.click(buttonWithText('Rose'));
  await creator.click(buttonWithText('Submit'));
  await creator.waitForSelector(ui('display-game'));

  await creator.screenshot({path: screenshotPath('creator', '04-2-waiting-for-others.png')});

  await player1.click(buttonWithText('Rose'));
  await player1.click(buttonWithText('Submit'));
  await player2.click(buttonWithText('Rose'));
  await player2.click(buttonWithText('Submit'));

  // place discs round
  await creator.waitForSelector(ui('disc-or-bid'));
  await creator.screenshot({path: screenshotPath('creator', '05-1-placing.png')});

  await player1.waitForSelector(ui('display-game'));
  await player1.screenshot({path: screenshotPath('player1', '05-1-display-game.png')});

  await player2.waitForSelector(ui('display-game'));
  await player2.screenshot({path: screenshotPath('player2', '05-1-display-game.png')});

  await creator.click(buttonWithText('Rose'));
  await creator.click(buttonWithText('Submit'));
  await creator.waitForSelector(ui('display-game'));
  await creator.screenshot({path: screenshotPath('creator', '05-2-player-1-placing.png')});

  await player1.waitForSelector(ui('disc-or-bid'));
  await player1.screenshot({path: screenshotPath('player1', '05-2-player-1-placing.png')});

  // player 1 opens the bidding with the maximum bid
  await player1.click(buttonWithText('4'));
  await player1.click(buttonWithText('Submit'));
  await player1.waitForSelector(ui('flip'));

  await creator.screenshot({path: screenshotPath('creator', '06-1-player-1-flipping.png')});
  await player1.screenshot({path: screenshotPath('player1', '06-1-player-1-flipping.png')});
  await player2.screenshot({path: screenshotPath('player2', '06-1-player-1-flipping.png')});

  // think about how to handle loading / messages lifecycle in WATs
//  await player1.click(buttonWithText('Flip'));
//  await player1.screenshot({path: screenshotPath('player1', '06-2-player-1-flipping.png')});
//  await player1.click(buttonWithText('Submit'));
//  await player1.screenshot({path: screenshotPath('player1', '06-3-player-1-flipping.png')});
//  await player1.click(buttonWithText('Flip'));
//  await player1.screenshot({path: screenshotPath('player1', '06-4-player-1-flipping.png')});
//  await player1.click(buttonWithText('Submit'));
//  await player1.screenshot({path: screenshotPath('player1', '06-5-player-1-flipping.png')});
//  await player1.click(buttonWithText('Flip'));
//  await player1.screenshot({path: screenshotPath('player1', '06-6-player-1-flipping.png')});
//  await player1.click(buttonWithText('Submit'));
//  await player1.screenshot({path: screenshotPath('player1', '06-7-player-1-flipping.png')});
//  await player1.click(buttonWithText('Flip'));
//  await player1.screenshot({path: screenshotPath('player1', '06-8-player-1-flipping.png')});
//
//  // round finished
//  await creator.screenshot({path: screenshotPath('creator', '07-1-player-1-success.png')});
//  await player1.screenshot({path: screenshotPath('player1', '07-1-player-1-success.png')});
//  await player2.screenshot({path: screenshotPath('player2', '07-1-player-1-success.png')});

  // Does not work - can't click export from automated browser?!
//  await creator.click(elmDebuggerButton());
//  const creatorDebuggerPopup = creatorContext.pages()[1];
//  await creatorDebuggerPopup.waitForLoadState('load');
//  await creatorDebuggerPopup.screenshot({path: screenshotPath('creator', '06-1-debugger.png')});
//  await creatorDebuggerPopup.on('download', download => {
//    console.log(download);
//    download.path().then(console.log)
//  });
//  await creatorDebuggerPopup.click("//button/span[contains(text(), 'Export')]/..");
//
//  await player1.click(elmDebuggerButton());
//  const player1DebuggerPopup = player1Context.pages()[1];
//  await player1DebuggerPopup.waitForLoadState('load');
//  await player1DebuggerPopup.on('download', download => download.path().then(console.log));
//  await player1DebuggerPopup.click("//button/span[contains(text(), 'Export')]/..");
//
//  await player2.click(elmDebuggerButton());
//  const player2DebuggerPopup = player2Context.pages()[1];
//  await player2DebuggerPopup.waitForLoadState('load');
//  await player2DebuggerPopup.on('download', download => download.path().then(console.log));
//  await player2DebuggerPopup.click("//button/span[contains(text(), 'Export')]/..");
//
//  await sleep(2000);
//  await creatorDebuggerPopup.close();
//  await player1DebuggerPopup.close();
//  await player2DebuggerPopup.close();

  await browser.close();
})();

function buttonWithText(text) {
  return "css=[role=button] >> text=" + text + "";
}

function labelledInput(label) {
  return "xpath=//*[contains(text(), '" + label + "')]/../..//input";
}

function textFragment(content) {
  return "xpath=//*[contains(text(), '" + content + "')]";
}

function ui(uiName) {
  return 'css=.ui--' + uiName;
}

function uiHook(uiHookName) {
  return 'css=.ui-hook--' + uiHookName;
}

function screenshotPath(player, filename) {
  return 'screenshots/' + player + '/' + filename;
}

function elmDebuggerButton() {
  return "//body/div[last()]";
}

function sleep(millis) {
  return new Promise(resolve => setTimeout(resolve, millis));
}
