// Check if the potential solution would return the same results for the given entry
// Otherwise, the potential solution is invalid
export function isSolutionValidForEntry(entry, potentialSolution) {
    const invalidLetterCounts = {};

    for (let i = 0; i < potentialSolution.length; i++) {
        const solutionLetter = potentialSolution[i];
        const entryLetter = entry[i].letter;

        // We only want to consider the counts of letters that are not fully correct
        if (solutionLetter === entryLetter) {
            continue;
        }

        if (solutionLetter in invalidLetterCounts) {
            invalidLetterCounts[solutionLetter] += 1;
        } else {
            invalidLetterCounts[solutionLetter] = 1;
        }
    }

    for (let i = 0; i < potentialSolution.length; i++) {
        const solutionLetter = potentialSolution[i];
        const currentEntry = entry[i];
        const entryLetter = currentEntry.letter;
        const entryState = currentEntry.state;

        if (solutionLetter === entryLetter) {
            // The solution letter matches the entry letter in the same position
            if (entryState !== 2) { // 2 = fully correct
                return false;
            }
        } else if (
            potentialSolution.includes(entryLetter) &&
            entryLetter in invalidLetterCounts &&
            invalidLetterCounts[entryLetter] > 0
        ) {
            // The potential solution includes the entry letter, but does not match the position
            if (entryState !== 1) { // 1 = incorrect position
                return false;
            }
            invalidLetterCounts[entryLetter] -= 1;
        } else {
            // There should be no more remaining occurrences of the letter in the potential solution
            if (entryState !== 0) { // 0 = not present
                return false;
            }
        }
    }

    return true;
}
