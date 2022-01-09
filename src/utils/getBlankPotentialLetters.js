export function getBlankPotentialLetters(numLetters=5) {
    let potentialLetters = {};
    for (let pos = 0; pos < numLetters; pos++) {
        potentialLetters[pos] = [];
        for (let char = 97; char < 123; char++) {
            potentialLetters[pos].push(String.fromCharCode(char));
        }
    }
    return potentialLetters;
}
