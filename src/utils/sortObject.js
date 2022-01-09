export function sortObject(obj) {
    let sorted = {};
    let keys = Object.keys(obj).sort(function(a, b) { return obj[b] - obj[a] });
    for (let i=0; i<keys.length; i++) {
        sorted[keys[i]] = obj[keys[i]];
    }
    return sorted;
}