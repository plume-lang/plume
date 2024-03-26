def add(x, y):
    return x + y

def mul(x, y):
    return x * y

def sub(x, y):
    return x - y

def eq(x, y):
    return x == y

def facto(n):
    match n:
        case 0:
            return 1
        case _:
            return n * facto(n-1)
    
print(facto(25))
