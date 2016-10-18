function len(lista){

    if (lista == []) {
        return 0;
    }
    
    else {
        return 1 + len(lista.tail);
    }
}

len([12312,312,435,6,67,8,69,65]);