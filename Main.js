//var x = 1, y = 10;
//x = x + y;

//testando o break dentro do blockstatement
/*if (true) {
	break;
	var x = 8; 
}
*/

//testando if-else
// var a = 1;
// break;
// if(a>1){
// 	var b = 2;

// }else{
// 	break;
// 	var c=3;
// }


// testar o DoWhile
// var f = 0;
// var i = 0;
//  while (i<3){
//  	break; //isso aqui acho que não tah pegando
//  	f = f-1;
//  	i = i+3;
//  }

/*
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

var x = [1, 2];
var y = [4, 5, 5.2];
*/

// var i = 0;

// for (;;) {
// 	f = f - 1;
// 	i = i + 1;
// 	if (i > 5) {break;};
// };
// f;

/*

var a = [true, 1];
var b = [true, 1];
if (a != b){
	f = f + 1;
}
f;
a != b;
*/

/*
function peppa(i) {
	if (i > 2) {
		return peppa(i-1);
	}
	return i + 10;
}
peppa(10);
*/

//testando o For
/*
for (;;) {
	f = f + 1;
	i = i + 1;
		break;
	
};
f;
*/

/*var a = [1,3,5,4,2,4,0];
a.head;
//a.tail;
a.len;

//teste funcao recursiva
/*
function fat(num) {
    if(num < 2) {
        return 1;
    }
    return fat(num-1)*num;
}
fat(4);
*/
/*
var x = [2,4];
var y = concat (x,[1,3]);
y;*/

// QUICKSORT TAH PEGANDOOOO
/*  
function quicksort (array){
    var less = [];
    var equal = [];
    var greater = [];
    var len = array.len;
    var pivot = array.head;
 
    if (len == 0){
        return [];
    }else{
 
        var i = 0;
        while(i<len) {
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
            i=i+1;
        }
    }
    less = quicksort(less);
    greater = quicksort(greater);
    var aux = less.concat(equal);
    array = aux.concat(greater);
    return array;
}
var x = quicksort([3,4,1,5,9]);
x;
*/


function repeteNumero(num){
	var list = [];
	var i = 0;
	while(i<num){
		list = list.concat(num);
		i = i+1;
	}
	return list;
} 
repeteNumero(3);

