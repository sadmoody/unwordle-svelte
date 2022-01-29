<script>
	import { wordle_answers } from './words.js';
	import { getBlankLetterFrequency } from './utils/getBlankLetterFrequency.js';
	import { isSolutionValidForEntry } from './utils/isSolutionValidForEntry.js';
	import { sortObject } from './utils/sortObject.js';

	let WORDLENGTH = 5;
	let showInstructions = false;
	let darkMode = false;

	let relevanceScores = {};
	let filteredAnswers = wordle_answers;
	let letterFrequency = getBlankLetterFrequency();
	let entries=[[]];

	function runAnalysis() {
		filteredAnswers = wordle_answers;

		filteredAnswers = filteredAnswers.filter(word => {
			let keepWord = true;

			for (let entryIndex = 0; entryIndex < entries.length - 1; entryIndex++) {
				const entry = entries[entryIndex];

				if (!isSolutionValidForEntry(entry, word)) {
					keepWord = false;
					break;
				}
			}

			return keepWord;
		});

		letterFrequency = getBlankLetterFrequency();
		filteredAnswers.forEach(word => {
			for (let pos=0; pos<word.length; pos++) {
				letterFrequency[pos][word[pos]]++;
			}
		});

		relevanceScores = {};
		filteredAnswers.forEach(word => {
			relevanceScores[word] = 0;
			for (let pos=0; pos<word.length; pos++) {
				relevanceScores[word] += letterFrequency[pos][word[pos]];
			}
		});
		relevanceScores = sortObject(relevanceScores);
	}

	function keyPress(key, keyCode){
		if (keyCode == 8) {
			entries[entries.length-1] = entries[entries.length-1].slice(0, -1);
		} else if (keyCode >= 65 && keyCode <= 90 && entries[entries.length-1].length < WORDLENGTH) {
			entries[entries.length-1] = [...entries[entries.length-1], {letter: key, state: -1}];
		} else if (keyCode === 13 && entries[entries.length-1].length === WORDLENGTH) {
			for (let pos=0; pos<entries[entries.length-1].length; pos++) {
				entries[entries.length-1][pos].state = 0;
			}
			entries = [...entries, []];
			runAnalysis();
		}
	}

	function handleKeydown(event) {
		let key = event.key;
		let keyCode = event.keyCode;
		keyPress(key, keyCode);
	}

	function keyboardEntry(key) {
		if (key === "Backspace") {
			keyPress(key, 8);
		} else if (key === "Enter") {
			keyPress(key, 13);
		} else {
			keyPress(key, key.charCodeAt(0)-32);
		}
	}

	function tilePress(e_index, l_index) {
		if (e_index < entries.length-1) {
			entries[e_index][l_index].state = (entries[e_index][l_index].state + 1) % 3;
			runAnalysis();
		}
	}
	
	function getTileStateName(state) {
		if (state === 0) {
			return "absent";
		} else if (state === 1) {
			return "present";
		} else if (state === 2) {
			return "correct";
		} else if (state === -1) {
			return "tbd";
		}
		return "empty";
	}
	function isDarkModeOn(state) {
		if (state === 1) {
			return "darkMode";
		} else {
			return "";
		}
	}

</script>

<svelte:window on:keydown={handleKeydown} on:load="{runAnalysis}"/>
<div id="game">
	<header>
		<div class="menu">
			<button id="help" class="icon" on:click={() => showInstructions = !showInstructions}>
				<game-icon icon="help">
					<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24">
						<path fill="var(--color-tone-3)" d="M11 18h2v-2h-2v2zm1-16C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.41 0-8-3.59-8-8s3.59-8 8-8 8 3.59 8 8-3.59 8-8 8zm0-14c-2.21 0-4 1.79-4 4h2c0-1.1.9-2 2-2s2 .9 2 2c0 2-3 1.75-3 5h2c0-2.25 3-2.5 3-5 0-2.21-1.79-4-4-4z"></path>
					</svg>
				</game-icon>
			</button>
		</div>
		<div class="title {darkMode ? 'darkModeText': ''}"> UNWORDLE </div>
		<div class="darkMode">
			<input
				type="checkbox"
				id="darkMode"
				on:click={() => { 
					darkMode = !darkMode;
					const body = document.getElementsByTagName("body");
					if (darkMode) {
						body[0].style.background="#121213";
					} else {
						body[0].style.background="#FFFFFF";
					}
				}}
			/>
			<label style="display:inline" class="{darkMode ? 'darkModeText': ''} ">Dark Mode</label>
		</div>
	</header>
	<div id="board-container">
		<div id="board" style="width: 350px; height: 420px;">
			{#each entries as entry, e_index}
				<game-row length={WORDLENGTH}>
					<div class="row">
						{#each entry as tile, l_index}
							<game-tile on:click={() => tilePress(e_index, l_index)}>
								<div class="tile" data-state="{getTileStateName(tile.state)}" data-animation="pop">{tile.letter}</div>
							</game-tile>
						{/each}
						{#each {length: WORDLENGTH - entry.length} as _}
							<game-tile>
								<div class="tile" data-state="empty"></div>
							</game-tile>
						{/each}
					</div>
				</game-row>
			{/each}
			<div class="suggestions">
				<div class="suggestion {darkMode ? 'darkModeText': ''}" id="title">Best words to guess next</div>
				{#each Object.entries(relevanceScores) as [word, score], index}
					{#if index < 10}
						<div class="suggestion {darkMode ? 'darkModeText': ''}">{index+1}. {word}</div>
					{/if}
				{/each}
			</div>
		</div>
	</div>
	<game-keyboard>
		<div id="keyboard">
			<div class="row">
				<button on:click={() => keyboardEntry("q")}>q</button>
				<button on:click={() => keyboardEntry("w")}>w</button>
				<button on:click={() => keyboardEntry("e")}>e</button>
				<button on:click={() => keyboardEntry("r")}>r</button>
				<button on:click={() => keyboardEntry("t")}>t</button>
				<button on:click={() => keyboardEntry("y")}>y</button>
				<button on:click={() => keyboardEntry("u")}>u</button>
				<button on:click={() => keyboardEntry("i")}>i</button>
				<button on:click={() => keyboardEntry("o")}>o</button>
				<button on:click={() => keyboardEntry("p")}>p</button>
			</div>
			<div class="row">
				<div class="spacer half"></div>
				<button on:click={() => keyboardEntry("a")}>a</button>
				<button on:click={() => keyboardEntry("s")}>s</button>
				<button on:click={() => keyboardEntry("d")}>d</button>
				<button on:click={() => keyboardEntry("f")}>f</button>
				<button on:click={() => keyboardEntry("g")}>g</button>
				<button on:click={() => keyboardEntry("h")}>h</button>
				<button on:click={() => keyboardEntry("j")}>j</button>
				<button on:click={() => keyboardEntry("k")}>k</button>
				<button on:click={() => keyboardEntry("l")}>l</button>
				<div class="spacer half"></div>
			</div>
			<div class="row">
				<button class="one-and-a-half" on:click={() => keyboardEntry("Enter")}>enter</button>
				<button on:click={() => keyboardEntry("z")}>z</button>
				<button on:click={() => keyboardEntry("x")}>x</button>
				<button on:click={() => keyboardEntry("c")}>c</button>
				<button on:click={() => keyboardEntry("v")}>v</button>
				<button on:click={() => keyboardEntry("b")}>b</button>
				<button on:click={() => keyboardEntry("n")}>n</button>
				<button on:click={() => keyboardEntry("m")}>m</button>
				<button class="one-and-a-half" on:click={() => keyboardEntry("Backspace")}>
					<game-icon icon="backspace">
						<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24">
							<path fill="var(--color-tone-1)" d="M22 3H7c-.69 0-1.23.35-1.59.88L0 12l5.41 8.11c.36.53.9.89 1.59.89h15c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2zm0 16H7.07L2.4 12l4.66-7H22v14zm-11.59-2L14 13.41 17.59 17 19 15.59 15.41 12 19 8.41 17.59 7 14 10.59 10.41 7 9 8.41 12.59 12 9 15.59z"></path>
						</svg>
					</game-icon>
				</button>
			</div>
		</div>
	</game-keyboard>
	<game-page style="display:{showInstructions ? "block":"none"}">
		<strong>HOW TO USE</strong>
		<game-help page slot="content">
			<section>
				<div class="instructions">
					<p>You should already know what <strong><a href="https://www.powerlanguage.co.uk/wordle/">WORDLE</a></strong> is if you're here. It's the word-game sensation by <a href="https://www.powerlanguage.co.uk/">Josh Wardle</a> taking the world by storm.</p>
					<p>This is a tool to help you pick the best next answer.</p>
					<p>After entering a word, click on the tiles to flip them to how they appear on Wordle.</p>
					<p>This website is purposefully "unplayable". It is just a tool to help you when you're stuck. If you want to play Wordle, go to the <a href="https://www.powerlanguage.co.uk/wordle/">THE OFFICIAL WEBSITE</a></p>
					<p>The interface is clunky and sloppy because I don't know what I'm doing.</p>
					<p><strong><a href="https://twitter.com/moodyhikmet">I'm on Twitter as well</a></strong></p>
					<p>You can find the source code for this site at this <strong><a href="https://github.com/sadmoody/unwordle-svelte">Github Repo</a></strong></p>
					<p>Yes if you use this website when you're playing - you're cheating. This is supposed to be a poor-man's Stockfish but for Wordle.</p>
				</div>
			</section>
		</game-help>
	</game-page>
</div>

<style>
	.instructions {
		font-size: 14px;
		color: var(--color-tone-1);
	}
	game-help section {
		padding: 16px;
		padding-top: 0px;
	}
	game-page {
		display: block;
		position: absolute;
		top: var(--header-height);
		background: white;
		max-width: var(--game-max-width);
		height:100%;
	}
	section {
		display: block;
	}
	.content {
		position: relative;
		color: var(--color-tone-1);
		padding: 0 32px;
		max-width: var(--game-max-width);
		width: 100%;
		overflow-y: auto;
		height: 100%;
		display: flex;
		flex-direction: column;
	}
	button.icon {
		background: none;
		border: none;
		cursor: pointer;
	}
	.suggestion#title {
		margin-bottom: 5px;
		font-size: larger;
	}
	.suggestion {
		font-family: inherit;
		font-weight: bold;
		text-transform: uppercase;
		text-align: center;
		height:20px;
		display:block;
	}
	.one-and-a-half {
		flex: 1.5;
		font-size: 12px;
	}
	.half {
		flex: 0.5;
	}
	#keyboard .row button {
		font-family: inherit;
		font-weight: bold;
		border: 0;
		padding: 0;
		margin: 0 6px 0 0;
		height: 58px;
		border-radius: 4px;
		cursor: pointer;
		user-select: none;
		background-color: var(--key-bg);
		color: var(--key-text-color);
		flex: 1;
		display: flex;
		justify-content: center;
		align-items: center;
		text-transform: uppercase;
		-webkit-tap-highlight-color: rgba(0,0,0,0.3);
	}
	#keyboard .row {
		display: flex;
		width: 100%;
		margin: 0 auto 8px;
		touch-action: manipulation;
	}
	#keyboard {
		margin: 0 8px;
		user-select: none;
	}
	game-keyboard {
		height: var(--keyboard-height);
	}
	header .title {
		font-weight: 700;
		font-size: 36px;
		letter-spacing: 0.2rem;
		text-transform: uppercase;
		text-align: center;
	}
	header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		height: var(--header-height);
		color: var(--color-tone-1);
		border-bottom: 1px solid var(--color-tone-4);
	}
	#game {
		width: 100%;
		max-width: var(--game-max-width);
		margin: 0 auto;
		height: 100%;
		display: flex;
		flex-direction: column;
	}
	#board-container {
		display: flex;
		justify-content: center;
		align-items: center;
		flex-grow: 1;
	}
	#board {
		display: grid;
		grid-template-rows: repeat(6, 1fr);
		grid-gap: 5px;
		padding: 10px;
		box-sizing: border-box;
		width: 100%;
		overflow:auto;
	}
	.row {
		display: grid;
		grid-template-columns: repeat(5, 1fr);
		grid-gap: 5px;
	}

	.tile[data-animation='flip-in'] {
		animation-name: FlipIn;
		animation-duration: 250ms;
		animation-timing-function: ease-in;
	}
	@keyframes FlipIn {
		0% {
			transform: rotateX(0);
		}
		100% {
			transform: rotateX(-90deg);
		}
	}

	.tile[data-animation='flip-out'] {
		animation-name: FlipOut;
		animation-duration: 250ms;
		animation-timing-function: ease-in;
	}
	@keyframes FlipIn {
		0% {
			transform: rotateX(-90deg);
		}
		100% {
			transform: rotateX(0);
		}
	}

	.tile[data-animation='pop'] {
		animation-name: PopIn;
		animation-duration: 100ms;
	}
	@keyframes PopIn {
		from {
			transform: scale(0.8);
			opactiy: 0;
		}

		40% {
			transform: scale(1.1);
			opacity: 1;
		}
	}

	.tile[data-state='absent'] {
		background-color: var(--color-absent);
	}
	.tile[data-state='correct'] {
		background-color: var(--color-correct);
	}
	.tile[data-state='present'] {
		background-color: var(--color-present);
	}
	.tile[data-state='tbd'] {
		background-color: var(--color-tone-7);
		border: 2px solid var(--color-tone-3);
		color: var(--color-tone-1);
	}
	.tile[data-state='empty'] {
    	border: 2px solid var(--color-tone-4);
	}
	.tile {
		width: 100%;
		height: 62px;
		display: inline-flex;
		justify-content: center;
		align-items: center;
		font-size: 2rem;
		line-height: 2rem;
		font-weight: bold;
		vertical-align: middle;
		box-sizing: border-box;
		color: var(--tile-text-color);
		text-transform: uppercase;
		user-select: none;
		opacity: 1;
	}
	.darkModeBackground {
		background-color: #121213;
	}

	.darkModeText {
		color: #d7dadc;
	}

	:root {
		--color-tone-1: #1a1a1b;
		--color-tone-2: #787c7e;
		--color-tone-3: #878a8c;
		--color-tone-4: #d3d6da;
		--color-tone-5: #edeff1;
		--color-tone-6: #f6f7f8;
		--color-tone-7: #ffffff;
		--opacity-50: rgba(255, 255, 255, 0.5);
	}

	:root {
		--green: #6aaa64;
		--darkendGreen: #538d4e;
		--yellow: #c9b458;
		--darkendYellow: #b59f3b;
		--lightGray: #d8d8d8;
		--gray: #86888a;
		--darkGray: #939598;
		--white: #fff;
		--black: #212121;
		--orange: #f5793a;
		--blue: #85c0f9;
		font-family: 'Clear Sans', 'Helvetica Neue', Arial, sans-serif;
		font-size: 16px;
		--header-height: 50px;
		--keyboard-height: 200px;
		--game-max-width: 500px;
	}
	:root {
        --color-present: var(--yellow);
        --color-correct: var(--green);
        --color-absent: var(--color-tone-2);
        --tile-text-color: var(--color-tone-7);
        --key-text-color: var(--color-tone-1);
        --key-evaluated-text-color: var(--color-tone-7);
        --key-bg: var(--color-tone-4);
        --key-bg-present: var(--color-present);
        --key-bg-correct: var(--color-correct);
        --key-bg-absent: var(--color-absent);
        --modal-content-bg: var(--color-tone-7);
    }
</style>