function mergesort(list) {
    var k;
    var l = list;
    k = l.len / 2;
    if (k < 1) {
        return l;
    }
    var left = [];
    var right = [];
    while (l.len > k) {
        left = left.concat(l.head);
        l = l.tail;
    }
    while (l.len > 0) {
        right = right.concat(l.head);
        l = l.tail;
    }
    left = mergesort(left);
    right = mergesort(right);
    var count = 0;
    while ((left.len > 0) && (right.len > 0)) {
        count = count + 1;
        if (left[0] < right[0]) {
            l = l.concat(left.head);
            left = left.tail;
        } else {
            l = l.concat(right.head);
            right = right.tail;
        }
    }
    while (left.len > 0) {
        l = l.concat(left.head);
        left = left.tail;
    }
    while (right.len > 0) {
        l = l.concat(right.head);
        right = right.tail;
    }
    return l;
}

var x = mergesort([3,0,3,34,3,43,43,4,344,1,5,9]);
x;

-----------------------------------

-- Teste funcao recursiva

function fatorial(num) {
    if(num < 2) {
        return 1;
    }
    return fatorial(num-1)*num;
}
fatorial(4);

----------------------------------
-- Teste for e break

var num = 2;
for (var i = 0; i < 10; ) { 
	if (i ==3) { 
		break;
	}
	num = num+2;
	i=i+1; 
} 
num;

--------------------------------
-- if / if-else 
var workingHard = true;
if (workingHard){
	pass = true;
}

//testando if-else com break ou sem break
var a = 3;
var b = 0;
if(a<1){
 	b=1;
}else{
	//break;
	var c=5;
}

------------------------------------

function repeteNumero(num){
	var list = [];
	var i = 0;
	while(i<num){
		list = list.concat(num);
		i = i+1;
	}
	return list;
} 
repeteNumero(5);

-----------------------------------

var x = []
 
function repeat(num) {
    for (var i = 0; i < num; i = i + 1) {
        x = x.concat(num);     
    }
}
 
function repeatAll(num) {
    for (var i = num; i >= 0; i = i - 1) {
        repeat(i);
    }
}
 
repeatAll(4);
x;

------------------------------------
function quicksort (array){
    var less = [];
    var equal = [];
    var greater = [];
    var len = array.len;
    var pivot = array.head;
 
    if (len == 0){
        return [];
    }else{
       
    for(var i=0;i<len;i = i+1){
        var temp = array[i];
        if (temp < pivot){
            less = less.concat(temp);
        }
        if (temp == pivot){
            equal = equal.concat(temp);
        }
        if (temp>pivot){
            greater = greater.concat(temp);
        }
       
    }
    }
    less = quicksort(less);
    greater = quicksort(greater);
    var aux = less.concat(equal);
    array = aux.concat(greater);
    return array;
}

------------------------------------
-- doWhile
var f = 0;
var i = 0;

do {
    f = f - 1;
    i = i + 1;
    if (i > 5) {
        break;
    };
 } while (i < 10);
 f;

------------------------------------
-- .head e .tail
var lista = [1,2,3,4];

function inverte(array){
    if (array == []) {
        return [];
    } else {
        var cabeca = array.head;
        var cauda = array.tail;
        return (inverte(cauda).concat(cabeca));
    }
}
inverte(lista);


---------------------------
function len(lista){

    if (lista == []) {
        return 0;
    }
    
    else {
        return 1 + len(lista.tail);
    }
}

len([12312,312,435,6,67,8,69,65]);





//var x = quicksort([3,4,1,5,9]);