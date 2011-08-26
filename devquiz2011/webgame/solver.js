function find(array, n) {
    for (var m = 0; m < array.length; ++m) {
        if (m != n && array[m] == array[n])
            return m;
    }
    return -1;
}

var s = document.getElementsByTagName('script').item(2).innerHTML.match(/\[.+\]/);
var array = eval('(' + s + ')');
var answer = [];
for (var n = 0; n < array.length; ++n) {
    answer[n] = find(array, n);
}

var form = document.getElementById('solve');
form.answer.value = answer.join(',');
form.submit();
