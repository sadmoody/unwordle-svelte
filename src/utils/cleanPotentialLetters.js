export function cleanPotentialLetters(entry, potentialLetters, numLetters=5){
    for (let pos=0; pos<numLetters; pos++){
        if (entry[pos].state === 0){
            for (let _pos=0; _pos<numLetters; _pos++){
                if (potentialLetters[_pos].includes(entry[pos].letter)){
                    potentialLetters[_pos] = potentialLetters[_pos].filter(letter => letter !== entry[pos].letter);
                }
            }
        } else if (entry[pos].state === 1){
            potentialLetters[pos] = potentialLetters[pos].filter(letter => letter !== entry[pos].letter);
        } else if (entry[pos].state === 2) {
            potentialLetters[pos] = [entry[pos].letter];
        }
    }
    return potentialLetters;
}