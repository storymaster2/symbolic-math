def leadingZeroesAfterDecimal(theNumber):
    if theNumber == 0:
        return 0

    theNumber = abs(theNumber)
    digit = 0

    while True:
        digit += 1
        if theNumber * pow(10, digit) > 1:
            return digit - 1

def formatFloat(theFloat, precision):
    if theFloat == 0:
        return ""
    else:
        return "{:.{prec}f}".format(theFloat, prec = precision + leadingZeroesAfterDecimal(theFloat)).rstrip('0').rstrip('.')

def gcd2(a, b):
    if isinstance(a, PolynomialExpression):
        if not(isinstance(b, PolynomialExpression)):
                raise Exception("inconsistent arguments: arguments shall all be of the same type")
        polynomialGCD = Euclidian(a, b)
        return polynomialGCD / content(polynomialGCD)
    else:
        DECIMAL_PRECISION_TO_KEEP = 6
        a = int(a * pow(10, DECIMAL_PRECISION_TO_KEEP))
        b = int(b * pow(10, DECIMAL_PRECISION_TO_KEEP))
        while not(a == 0):
            temp = a
            a = b % a
            b = temp
        return b / pow(10, DECIMAL_PRECISION_TO_KEEP)

def combinations(array, tuple_length, prev_array=[]):
    if len(prev_array) == tuple_length:
        return [prev_array]
    combs = []
    for i, val in enumerate(array):
        prev_array_extended = prev_array.copy()
        prev_array_extended.append(val)
        combs += combinations(array[i+1:], tuple_length, prev_array_extended)
    return combs

def combinationsFrozen(array, tuple_length):
    sets = []
    for comb in combinations(array, tuple_length):
        sets.append(frozenset(comb))

    return sets