export function getBlankLetterFrequency(numLetters=5){
    let letterFrequency = {};
    for (let pos=0; pos<numLetters; pos++){
        letterFrequency[pos] = {};
        for (let char=97; char<123; char++){
            letterFrequency[pos][String.fromCharCode(char)] = 0;
        }
    }
    return letterFrequency;
}