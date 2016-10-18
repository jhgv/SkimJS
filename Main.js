function merge(a, b)
{
    var result = [],
        k = 0,
        i = 0,
        j = 0;

    while(a.len > i && b.len > j){
        if(a[i] <= b[j]){
            result[k++] = a[i++];
        } else {
            result[k++] = b[j++];
        }
    }

    while(a.len > i) {
        result[k++] = a[i++];
    }

    while(b.len > j) {
        result[k++] = b[j++];
    }

    return result;
}