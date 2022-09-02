from collections.abc import Iterable
import math
import copy
import time
from fractions import Fraction

def toFraction(value):
    if value == None:
        return None
    if isinstance(value, Fraction):
        return value
    if isinstance(value, Iterable) and not(isinstance(value, str)):
        returnList = []
        for x in value:
            if isinstance(x, (Fraction, int, float)):
                returnList.append(Fraction(x))
            else:
                returnList.append(x)
        return returnList
    # elif isinstance(value, Fraction):
    #     return value
    elif isinstance(value, (int, float)):
        return Fraction(value)
    else:
        return value

def leadingZeroesAfterDecimal(theNumber):
    if theNumber == 0:
        return 0

    theNumber = abs(theNumber)
    digit = 0

    while True:
        digit += 1
        if theNumber * pow(10, digit) > 1:
            return digit - 1

def formatNumberOutput(number, precision):
    if number == 0:
        return ""
    else:
        return "{:.{prec}f}".format(number, prec = precision + leadingZeroesAfterDecimal(number)).rstrip('0').rstrip('.')

def content(A):
    if not(isinstance(A, PolynomialExpression)):
        raise Exception("input A must be a polynomial")

    coefficients = [i for i in A.getCoefficientList() if i != 0]
    return gcd(coefficients)

def gcd2(a, b):
    if isinstance(a, PolynomialExpression):
        if not(isinstance(b, PolynomialExpression)):
            raise Exception("inconsistent arguments: arguments shall all be of the same type")
        if a.degree < b.degree:
            temp = a
            a = b
            b = temp
        while b != 0:
            remainder = a % b
            if remainder == a:
                return None
            a = b
            b = remainder

        return a / content(a)
    else:
        DECIMAL_PRECISION_TO_KEEP = 6
        a = int(a * pow(10, DECIMAL_PRECISION_TO_KEEP))
        b = int(b * pow(10, DECIMAL_PRECISION_TO_KEEP))
        while not(a == 0):
            temp = a
            a = b % a
            b = temp
        return b / pow(10, DECIMAL_PRECISION_TO_KEEP)

def gcd(*args):
    input = []
    for element in args:
        if (isinstance(element, list)):
            input.extend(element)
        else:
            input.append(element)

    result = input[0]
    for n in input[1:]:
        result = gcd2(n, result)
    return result

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

def integerRoot(value, exponent):
    if exponent < 0:
        raise Exception("invalid input - exponent shall be greater than 0")
    if exponent >= 1:
        return value ** exponent
    if exponent == 1:
        return 1
    
    e = 1 / exponent

    n = 1
    while n ** e <= value:
        if n ** e == value:
            return n
        n += 1
    
    return None

def sqrt(value):
    return IrrationalRoot(value, Fraction(1,2)).evaluate()

# End helper functions
# ------------------------------------------
# ------------------------------------------
# ------------------------------------------
# ------------------------------------------

class PolynomialSeries:
    terms = []

    def __init__(self, *args):
        self.terms = []

        for term in args:
            if isinstance(term, list):
                for i in range(len(term)):
                    if isinstance(term[i], PolynomialExpression):
                        self.terms.append(term[i])
                    if isinstance(term[i], (Coefficient, int, float)):
                        self.terms.append(PolynomialExpression(term[i]))
            else:
                if isinstance(term, PolynomialExpression):
                    self.terms.append(term)
                if isinstance(term, (Coefficient, int, float)):
                    self.terms.append(PolynomialExpression(term))
        
        self.cleanUp()

    def __str__(self):
        return self.toString()
    
    def __add__(self, other):
        return self.add(other)

    def __radd__(self, other):
        return self.__add__(other)

    def __iadd__(self, other):
        return self.add(other, overwriteSelf=False)

    def __sub__(self, other):
        return self.add(other * -1)

    def __rsub__(self, other):
        return self.__sub__(other)

    def __isub__(self, other):
        return self.add(other * -1, overwriteSelf=False)

    def __mul__(self, other):
        return self.multiply(other)
    
    def __rmul__(self, other):
        return self.__mul__(other)

    def __imul__(self, other):
        return self.multiply(other, overwriteSelf=True)

    def toString(self):
        returnString = ""
        for i in range(len(self.terms)):
            if len(returnString) > 0:
                returnString += "\n"
            returnString += self.terms[i].toString()
        if returnString == "":
            returnString = "0"
        return returnString

    def add(self, other, overwriteSelf=False):
        newTerms = self.terms
        if isinstance(other, PolynomialSeries):
            newTerms.extend(other.terms)

        elif isinstance(other, PolynomialExpression):
            newTerms.append(other)

        elif isinstance(other, (Coefficient, int, float)):
            newTerms.append(PolynomialExpression(other))

        else:
            return NotImplemented

        if overwriteSelf:
            self.terms = newTerms
            self.cleanUp()
            return self
        else:
            return PolynomialSeries(newTerms)

    def multiply(self, other, overwriteSelf=False):
        newTerms = []
        if isinstance(other, PolynomialSeries):
            for term_a in self.terms:
                for term_b in other.terms:
                    newTerms.append(term_a * term_b)

        elif isinstance(other, (PolynomialExpression, Coefficient, int, float)):
            newTerms = self.terms
            for term in newTerms:
                term *= other

        else:
            return NotImplemented

        if overwriteSelf:
            self.terms = newTerms
            self.cleanUp()
            return self
        else:
            return PolynomialSeries(newTerms)

    def cleanUp(self):

        # delete any zero items
        deleteMask = []
        for i in range(len(self.terms)):
            if self.terms[i].isZero():
                deleteMask.append(i)

        shift = 0
        for delete in deleteMask:
            del self.terms[delete - shift]
            shift += 1

    def simplify(self):
        for term in self.terms:
            term.simplify()
        return self

class PolynomialExpression:
    OUTPUT_PRECISION = 2
    terms = {}
    denominator = None

    def __init__(self, coefficients=Fraction(1), exponents=Fraction(0), denominatorCoefficients=None, denominatorExponents=None, terms=None, denominator=None):
        if terms != None:
            # override standard construction
            self.terms = copy.deepcopy(terms)
            self.denominator = copy.deepcopy(denominator)

        else:
            # typical construction
            coefficients = toFraction(coefficients)
            exponents = toFraction(exponents)
            denominatorCoefficients = toFraction(denominatorCoefficients)
            denominatorExponents = toFraction(denominatorExponents)

            if not(isinstance(coefficients, Iterable)):
                coefficients = [coefficients]

            if not(isinstance(exponents, Iterable)):
                exponents = [exponents]

            # check for valid input
            for c in coefficients:
                if not(isinstance(c, (Coefficient, Fraction))):
                    raise Exception("Invalid input")
            for e in exponents:
                if not(isinstance(e, Fraction)):
                    raise Exception("Invalid input")
            
            if len(coefficients) > len(exponents):
                raise Exception("Invalid input - list lengths must match")

            # Pad out coefficients list with zeros to match size of keys list, if needed
            for n in range(len(exponents) - len(coefficients)):
                coefficients.append(Fraction(0))

            self.terms = dict(zip(exponents, coefficients))

            if denominatorCoefficients != None or denominatorExponents != None:
                self.denominator = PolynomialExpression(denominatorCoefficients, denominatorExponents)

        self.cleanUp()

    def __str__(self):
        return self.toString()

    def __add__(self, other):
        return self.add(other)

    def __radd__(self, other):
        return self.__add__(other)

    def __iadd__(self, other):
        return self.add(other, overwriteSelf=False)

    def __sub__(self, other):
        return self.add(other * -1)

    def __rsub__(self, other):
        return self.__sub__(other)

    def __isub__(self, other):
        return self.add(other * -1, overwriteSelf=False)

    def __mul__(self, other):
        return self.multiply(other)
    
    def __rmul__(self, other):
        return self.__mul__(other)

    def __imul__(self, other):
        return self.multiply(other, overwriteSelf=True)

    def __truediv__(self, other):
        return self.divide(other)

    def __rtruediv__(self, other):
        if isinstance(other, (Coefficient, int, float)):
            otherExpression = PolynomialExpression(other)
            return otherExpression / self
        else:
            return NotImplemented

    def __itruediv__(self, other):
        return self.divide(other, overwriteSelf=True)

    def __floordiv__(self, other):
        # returns quotient of division, regardless of whether remainder is present
        Q_R = self.polyDivide(other)
        return Q_R[0]

    def __mod__(self, other):
        Q_R = self.polyDivide(other)
        return Q_R[1]

    def __pow__(self, other):
        return self.raiseToPower(other)

    def __eq__(self, other):
        if isinstance(other, PolynomialExpression):
            return NotImplemented
        elif isinstance(other, (int, float)):
            if other == 0:
                return self.isZero()
            else:
                if self.isNumber():
                    return self.terms[0] == other
                else:
                    return False
        elif isinstance(other, Coefficient):
            if other.isZero():
                return self.isZero()
            else:
                return NotImplemented
        else:
            return NotImplemented

    @property
    def degree(self):
        self.cleanUp()
        # For now, assume degree is always an integer
        maxDegree = max(self.terms.keys())
        if maxDegree % 1 != 0:
            raise Exception("for now, polynomial exponents must be integers")
        else:
            return int(maxDegree)

    @property
    def lc(self):
        #returns the leading coefficient
        #leading coefficient is the coefficient corresponding to the largest exponent
        return self.terms[self.degree]

    @property
    def content(self, coefficientVariable):
        coefficients = [i for i in self.getCoefficientList() if i != 0]

        result = coefficients[0]
        for n in coefficients[1:]:
            result = result.gcd2Coefficient(n, coefficientVariable)
        return result

    def cleanUp(self):
        for exp, coeff in self.terms.copy().items():
            if isinstance(coeff, Coefficient):
                if coeff.isZero():
                    del self.terms[exp]
                if coeff.isNumber():
                    self.terms[exp] = coeff.evaluate()
            elif isinstance(coeff, Fraction):
                if coeff == 0:
                    del self.terms[exp]
            else:
                raise Exception("Invalid data")

        if len(self.terms) == 0:
            self.terms[0] = Fraction(0)

    def setPrimaryCoefficientVariable(self, variable):
        for coeff in self.terms.values():
            if isinstance(coeff, Coefficient):
                coeff.setPrimaryVariable(variable)

    def hasCommonDivisor(self, A, B):
        commonDivisor = gcd2(A,B)
        if isinstance(commonDivisor, PolynomialExpression):
            if not(commonDivisor.isZero()) and not(commonDivisor.isUnity()):
                return True
        return False

    def gcd(self, b, coefficientVariable):
        if not(isinstance(b, PolynomialExpression)):
            raise Exception("invalid input")

        a = copy.deepcopy(self)

        # if necessary, swap expressions so that a has the higher degree
        if a.degree < b.degree:
            temp = a
            a = b
            b = temp

        while b != 0:
            remainder = a % b
            a = b
            b = remainder

        return a / content(a, coefficientVariable)

    def simplify(self, coefficientVariable=None):
        if self.hasDenominator():
            self.cleanUp()
            if self.hasDenominator():
                Q_R = self.polyDivide(self.denominator)
                if Q_R[1] == 0:
                    self.terms = Q_R[0].terms
                    self.denominator = None
                    self.cleanUp()

            if self.hasDenominator():
                commonDivisor = self.gcd(self.denominator, coefficientVariable)
                if not(commonDivisor == 1):
                    newNumerator = self.getNumerator() // commonDivisor
                    self.terms = newNumerator.terms
                    self.denominator = self.denominator // commonDivisor
                    self.cleanUp()
        
        for coeff in self.terms.values():
            if isinstance(coeff, Coefficient):
                coeff.simplify(coefficientVariable)

        return self

    def add(self, other, overwriteSelf=False):
        newTerms = copy.deepcopy(self.terms)
        newDenominator = copy.deepcopy(self.denominator)
        if isinstance(other, PolynomialExpression):
            if (self.hasDenominator() or other.hasDenominator()) and not self.denominator == other.denominator:
                return PolynomialSeries(self, other)

            else:
                for exp, coeff in other.terms.items():
                    if exp in newTerms:
                        if isinstance(coeff, Coefficient):
                            newTerms[exp] = coeff + newTerms[exp]
                        else:
                            newTerms[exp] += coeff
                    else:
                        newTerms[exp] = coeff

        elif isinstance(other, (Coefficient, int, float)):
            if self.hasDenominator():
                # Use cross multiplication
                numerator1 = PolynomialExpression(terms=self.terms)
                numerator2 = PolynomialExpression(terms=self.denominator.terms) * other
                combinedNumerator = numerator1 + numerator2
                newTerms = combinedNumerator.terms
            else:
                if Fraction(0) in newTerms:
                    newTerms[Fraction(0)] += other
                else:
                    newTerms[Fraction(0)] = other
        else:
            raise Exception("Invalid argument")
        
        return self.arithmeticReturnValue(newTerms, newDenominator, overwriteSelf)

    def multiply(self, other, overwriteSelf=False):
        newTerms = {}
        newDenominator = self.denominator
        if isinstance(other, PolynomialExpression):
            if other.hasDenominator():
                if self.hasDenominator():
                    newDenominator = self.denominator * other.denominator
                else:
                    newDenominator = other.denominator

            for exp_a, coeff_a in self.terms.items():
                for exp_b, coeff_b in other.terms.items():
                    if (exp_a + exp_b) in newTerms:
                        newTerms[exp_a + exp_b] += coeff_a * coeff_b
                    else:
                        newTerms[exp_a + exp_b] = coeff_a * coeff_b
        elif isinstance(other, (Coefficient, Fraction, int, float)):
            other = toFraction(other)
            for e in self.terms.keys():
                newTerms[e] = self.terms[e] * other
        else:
            raise Exception("Invalid argument")
        
        return self.arithmeticReturnValue(newTerms, newDenominator, overwriteSelf)

    def divide(self, other, overwriteSelf=False):
        if isinstance(other, PolynomialExpression):
            if other.isZero():
                raise Exception("Divide by zero error")
            elif other.isUnity():
                return self
            elif other.isNumber():
                return self.multiply(Fraction(1) / other, overwriteSelf=overwriteSelf)
            else:
                numerator = self.getNumerator()
                if other.hasDenominator():
                    numerator *= other.denominator
                if self.hasDenominator():
                    if overwriteSelf:
                        self.denominator *= other.getNumerator()
                        self.terms = numerator.terms
                        self.cleanUp()
                        return self
                    else:
                        return PolynomialExpression(terms=numerator.terms, denominator=self.denominator * other.getNumerator())
                else:
                    if overwriteSelf:
                        if PolynomialExpression(terms=numerator.terms) % other == 0:
                            numerator = PolynomialExpression(terms=numerator.terms) // other
                        else:
                            self.denominator = other.getNumerator()
                        self.terms = numerator.terms
                        self.cleanUp()
                        return self
                    else:
                        if PolynomialExpression(terms=numerator.terms) % other == 0:
                            return PolynomialExpression(terms=numerator.terms) // other
                        else:
                            return PolynomialExpression(terms=numerator.terms, denominator=other.getNumerator())
                    
        elif isinstance(other, (Coefficient, Fraction, int)):
            return self.multiply(Fraction(1) / other, overwriteSelf=overwriteSelf)
        else:
            return NotImplemented

    def arithmeticReturnValue(self, newTerms, newDenominator, overwriteSelf):
        if overwriteSelf:
            self.terms = newTerms
            self.denominator = newDenominator
            self.cleanUp()
            return self
        else:
            return PolynomialExpression(terms=newTerms, denominator=newDenominator)

    def polyDivide(self, other):
        # Euclidian Polynomial Division (aka polynomial long division)

        if not(isinstance(other, PolynomialExpression)):
            raise Exception("invalid type: input must be a PolynomialExpression")

        if other == 0:
            raise Exception("Divide by zero error")

        Q = PolynomialExpression(0,0)
        R = copy.deepcopy(self)
        d = R.degree - other.degree
        
        while R != 0 and (d >= 0):
            T = PolynomialExpression(R.lc / other.lc,d)
            Q += T
            R -= other * T
            d = R.degree - other.degree

        return([Q,R])

    def polyPseudoDivide(self, other):
        # Performs integer-only polynomial division

        if not(isinstance(self, PolynomialExpression)) or not(isinstance(other, PolynomialExpression)):
            raise Exception("inputs aren't PolynomialExpressions")

        if other == 0:
            raise Exception("Divide by zero error: B cannot be zero")
        
        b = other.lc
        N = self.degree - other.degree + 1
        pseudoQ = PolynomialExpression(0,0)
        pseudoR = self
        d = pseudoR.degree - other.degree
        expansionFactor = 1
        while pseudoR != 0 and (d >= 0):
            T = PolynomialExpression(pseudoR.lc,d)
            N -= 1
            pseudoQ = pseudoQ * b + T
            pseudoR = pseudoR * b - other * T
            d = pseudoR.degree - other.degree
            expansionFactor *= b
        
        return [pseudoQ, pseudoR, expansionFactor] # Note: answers are equal to the actual answer times b ** N (TODO: verify this)

    def toString(self):
        if self.isZero():
            return "0"
        else:
            returnString = ""
            if self.isNegative():
                returnString += "- "
            if self.hasDenominator():
                returnString += "[("
            returnString += self.numeratorString()
            if self.hasDenominator():
                returnString += ")/("
                returnString += self.denominatorString()
                returnString += ")]"
            return returnString

    def numeratorString(self):
        returnString = ""
        exponents = sorted(self.terms.keys(), reverse=True)
        for e in exponents:
            if isinstance(self.terms[e], Coefficient):
                if not(self.terms[e].isZero()):
                    returnString += self.terms[e].toString(parentheses=True, leadingOperator=not(len(returnString) == 0), returnUnity=(e==0))
                    if e > 0:
                        returnString += "x"
                    if e > 1:
                        returnString += "^"
                        returnString += str(e)
            elif isinstance(self.terms[e], Fraction):
                if self.terms[e] != 0:
                    if len(returnString) == 0:
                        if self.terms[e] < 0:
                            returnString += "-"
                    else:
                        if self.terms[e] < 0:
                            returnString += " - "
                        else:
                            returnString += " + "

                    returnString += str(abs(self.terms[e]))
                    if e > 0:
                        returnString += "x"
                    if e > 1:
                        returnString += "^"
                        returnString += str(e)
            else:
                raise Exception("Invalid data structure")
        return returnString

    def denominatorString(self):
        return self.denominator.toString()

    def isZero(self):
        for e, coeff in self.terms.items():
            if isinstance(coeff, Coefficient):
                if not(coeff.isZero()):
                    return False
            elif isinstance(coeff, Fraction):
                if coeff != 0:
                    return False
            else:
                raise Exception("Invalid data structure")

        return True

    def isUnity(self):
        for e, coeff in self.terms.items():
            if isinstance(coeff, Coefficient):
                if e == 0:
                    if not(coeff.isUnity()):
                        return False
                else:
                    if not(coeff.isZero()):
                        return False
            elif isinstance(coeff, Fraction):
                if e == 0:
                    if coeff != 1:
                        return False
                else:
                    if coeff != 0:
                        return False
            else:
                raise Exception("Invalid data structure")

        return True

    def isNumber(self):
        for e, coeff in self.terms.items():
            if isinstance(coeff, Coefficient):
                if e != 0:
                    if not(coeff.isZero()):
                        return False
            elif isinstance(coeff, Fraction):
                if e != 0 and coeff != 0:
                    return False
            else:
                raise Exception("coefficient value is not of supported type")
            
        if self.hasDenominator():
            if not(self.denominator.isNumber()):
                return False
        
        return True

    def isNegative(self):
        leadingCoefficient = self.degree
        if isinstance(leadingCoefficient, Coefficient):
            return leadingCoefficient.isNegative()
        elif isinstance(leadingCoefficient, (int, Fraction)):
            return leadingCoefficient < 0
        else:
            raise Exception("Invalid data structure")

    def hasDenominator(self):
        if self.denominator == None:
            return False
        else:
            if self.denominator.isUnity():
                self.denominator = None
                return False
            else:
                return True

    def getCoefficientList(self):
        #returns a list of the coefficients, ordered from highest degree to lowest
        coefficients = []
        for e in range(self.degree, -1, -1):
            if e in self.terms:
                coefficients.append(self.terms[e])
            else:
                coefficients.append(Fraction(0))

        return coefficients

    def getNumerator(self):
        return PolynomialExpression(terms=self.terms)

    def getDerivative(self):
        newCoefficients = []
        newExponents = []
        for exp, coeff in self.terms.items():
            newCoefficients.append(coeff * exp)
            newExponents.append(exp - 1)

        return PolynomialExpression(newCoefficients, newExponents)

    def raiseToPower(self, power):
        if power == 0:
            return Fraction(1)
        if isinstance(power, Fraction):
            if power % 1 != 0:
                raise Exception("non-integer exponents not implemented yet")
            else:
                power = int(power)
        newExpression = copy.deepcopy(self)
        for p in range(power - 1):
            newExpression *= self
        return newExpression

    def getNumerator(self):
        numerator = copy.deepcopy(self)
        numerator.denominator = None
        return numerator

    def getDenominator(self):
        if self.hasDenominator():
            return self.denominator()

    def convertToCoefficient(self, variable):
        returnCoefficient = Coefficient(Fraction(0))
        for e in self.terms.keys():
            if isinstance(self.terms[e], Coefficient):
                returnCoefficient += self.terms[e] * Symbol(variable, e)
            elif isinstance(self.terms[e], Fraction):
                returnCoefficient += Coefficient(self.terms[e], Symbol(variable, e))
            else:
                raise Exception("Invalid data structure")
        return returnCoefficient

    def evaluate(self, value):
        returnValue = Fraction(0)
        for e in self.terms.keys():
            returnValue += self.terms[e] * value ** e

        return returnValue

    def getRoots(self):
        if self.degree == 0:
            raise Exception("no roots found")
        elif self.degree == 1:
            if 0 in self.terms.keys():
                return -self.terms[0] / self.terms[1]
            else:
                return 0
        elif self.degree == 2:
            if 1 in self.terms.keys():
                # TODO: implement quadratic formula
                raise Exception("not implemented")
            else:
                if 0 in self.terms.keys():
                    underTheRoot = -self.terms[0] / self.terms[2]
                    return IrrationalRoot(underTheRoot, Fraction(1/2))
                else:
                    return 0
        elif self.degree == 3:
            if 1 in self.terms.keys() or 2 in self.terms.keys():
                # TODO: implement cubic root analysis
                raise Exception("not implemented")
            else:
                if 0 in self.terms.keys():
                    underTheRoot = -self.terms[0] / self.terms[3]
                    return IrrationalRoot(underTheRoot, Fraction(1/3))
                else:
                    return 0
        else:
            raise Exception("not implemented")

class Symbol:
    # Note: this class is intended to be an immutable object
    # This means that only the constructor should set state variables
    __IDENTITY = "_"
    __variable = ""
    __exponent = Fraction(1)

    def __init__(self, variable=None, exponent=Fraction(1)):
        exponent = toFraction(exponent)

        if variable == None or exponent == 0:
            self.__variable = self.__IDENTITY
        else:
            self.__variable = variable

        if variable == None or variable == self.__IDENTITY:
            self.__exponent = Fraction(0)
        else:
            self.__exponent = exponent
    
    def __str__(self):
        return self.toString()

    def __mul__(self, other):
        return self.multiply(other)

    def __truediv__(self, other):
        return self.divide(other)

    def __eq__(self, other):
        if isinstance(other, Symbol):
            return self.__variable == other.__variable and self.__exponent == other.__exponent
        elif isinstance(other, SymbolSet):
            return other == self
        else:
            return NotImplemented

    def __hash__(self):
        return hash((self.__variable, self.__exponent))

    @property
    def degree(self):
        if self.__variable == self.__IDENTITY:
            return Fraction(0)
        return self.__exponent

    @property
    def variable(self):
        if self.__variable == self.__IDENTITY:
            return ""
        return self.__variable

    @property
    def exponent(self):
        return self.__exponent

    def toString(self):
        if self.__variable == self.__IDENTITY:
            return ""
        else:
            if self.__exponent == 1:
                return self.__variable
            else:
                if self.exponent % 1 == 0:
                    return self.__variable + "^" + str(self.__exponent)
                else:
                    return self.__variable + "^(" + str(self.__exponent + ")")
    
    def multiply(self, other):
        if isinstance(other, Symbol):
            if self.__variable == other.__variable:
                return Symbol(self.__variable, self.__exponent + other.__exponent)
            else:
                return SymbolSet(self.__variable, self.__exponent) * other
        else:
            return NotImplemented

    def divide(self, other):
        if isinstance(other, Symbol):
            if self.__variable == other.__variable:
                return Symbol(self.__variable, self.__exponent - other.__exponent)
            else:
                return SymbolSet(self.__variable, self.__exponent) / other
        else:
            return NotImplemented

    def isIdentity(self):
        if self.__exponent == 0:
            return True
        if self.__variable == self.__IDENTITY:
            return True
        return False

    def equalsSameVariable(self, other):
        if isinstance(other, Symbol):
            return self.__variable == other.__variable
        else:
            return NotImplemented

    def evaluate(self, value):
        return toFraction(value) ** self.__exponent

    @property
    def symbolsList(self):
        return set(self)

class SymbolSet:
    # Note: this class is intended to be an immutable object
    # This means that only the constructor should set state variables
    __symbols = None

    def __init__(self, symbols=None, exponents=None):
        # Ensure that any exponents are represented as Fraction objects
        exponents = toFraction(exponents)

        symbolList = []
        if isinstance(symbols, Iterable) and not isinstance(symbols, str):
            for i in range(len(symbols)):
                if isinstance(symbols[i], Symbol):
                    symbolList.append(symbols[i])
                elif isinstance(symbols[i], str):
                    symbolList.append(symbols[i], exponents[i])
                else:
                    raise Exception("Invalid input")
        elif symbols != None:
            if isinstance(symbols, Symbol):
                symbolList.append(symbols)
            elif isinstance(symbols, str):
                if exponents == None:
                    exponents = Fraction(1)
                symbolList.append(Symbol(symbols, exponents))
            else:
                raise Exception("Invalid input")

        # Check for duplicate variables and combine them before freezing the set
        finalSymbolSet = set([Symbol()])
        usedVariables = set()
        for a in range(len(symbolList)):
            if not(symbolList[a].variable in usedVariables):
                usedVariables.add(symbolList[a].variable)
                for b in range(a + 1, len(symbolList)):
                    if symbolList[a].variable == symbolList[b].variable:
                        symbolList[a] = symbolList[a] * symbolList[b]
                finalSymbolSet.add(symbolList[a])

        self.__symbols = frozenset(finalSymbolSet)
    
    def __str__(self):
        return self.toString()

    def __mul__(self, other):
        return self.multiply(other)

    def __rmul__(self, other):
        return self.__mul__(other)

    def __truediv__(self, other):
        return self.divide(other)

    def __eq__(self, other):
        if other == None:
            return self.__symbols == None
        elif isinstance(other, Symbol):
            matchingSymbol = False
            for s in self.__symbols:
                if s != other and not(s.isIdentity()):
                    return False
                if s == other:
                    matchingSymbol = True
            return matchingSymbol
        elif isinstance(other, SymbolSet):
            return self.__symbols == other.__symbols
        else:
            return False

    def __hash__(self):
        return hash((self.__symbols, 1))

    def isIdentity(self):
        for s in self.__symbols:
            if not(s.isIdentity()):
                return False
        return True

    def toString(self):
        strings = []
        returnString = ""
        for s in self.__symbols:
            strings.append(str(s))
        for s in sorted(strings): # TODO: update sorting function to change ordering of exponents
            returnString += s
        return returnString
    
    def multiply(self, other):
        newSymbolList = list(self.__symbols)
        if isinstance(other, Symbol):
            newSymbolList.append(other)
        elif isinstance(other, SymbolSet):
            newSymbolList.extend(list(other.__symbols))
        else:
            return NotImplemented

        return SymbolSet(newSymbolList)

    def divide(self, other):
        newSymbolList = list(self.__symbols)
        if isinstance(other, Symbol):
            newSymbolList.append(Symbol(other.variable, Fraction(-1) * other.exponent))
        elif isinstance(other, SymbolSet):
            for s in list(other.__symbols):
                newSymbolList.append(Symbol(s.variable, Fraction(-1) * s.exponent))
        else:
            return NotImplemented

        return SymbolSet(newSymbolList)

    def evaluate(self, values, variables=None):
        if self.isIdentity():
            return Fraction(1)

        values = toFraction(values)
        product = Fraction(1)

        if isinstance(values, Iterable):
            if len(values) > 1:
                if variables == None:
                    raise Exception("cannot evaluate: list of values provided, but no corresponding list of symbols provided")
                if len(variables) != len(values):
                    raise Exception("cannot evaluate: length of values list does not match length of symbols list")

                for i in range(len(variables)):
                    for s in self.__symbols:
                        if s.variable == variables[i]:
                            product *= s.evaluate(values[i])
            else:
                if self.uniqueVariableCount > 1:
                    raise Exception("cannot evaluate: symbol set contains multiple variables but only one value provided")

                for s in self.__symbols:
                    product *= s.evaluate(values[0])
        
        elif isinstance(values, Fraction):
            if self.uniqueVariableCount > 1:
                raise Exception("cannot evaluate: symbol set contains multiple variables but only one value provided")

            for s in self.__symbols:
                product *= s.evaluate(values)

        else:
            raise Exception("Invalid data structure")

        return product

    def containsVariable(self, variable):
        return variable in self.variableList

    def getExponent(self, variable):
        if isinstance(variable, str):
            for s in self.__symbols:
                if s.variable == variable:
                    return s.exponent
            return Fraction(0)
        else:
            raise Exception("Invalid input - string expected")

    @property
    def uniqueVariableCount(self):
        return len(self.variableList)

    @property
    def variableList(self):
        return set([s.variable for s in self.__symbols if s.variable != ""])

    @property
    def symbolsSet(self):
        symbols = set(self.__symbols)
        if len(symbols) > 1:
            symbols.discard(Symbol())
        return symbols

class Number:
    # A data type that can include both rational and irrational numeric data (no symbols)
    rationalComponent = Fraction(0)
    irrationalComponent = None

    def __init__(self, rational=Fraction(0), irrational=None):
        self.rationalComponent = rational
        self.irrationalComponent = irrational
    
    def toString(self):
        if self.IrrationalComponent == None:
            return str(self.rationalComponent)
        
        if self.rationalComponent == 0:
            return str(self.irrationalComponent)

        returnString = "(" + str(self.rationalComponent)
        return "(" + str(self.rationalComponent) + " "

class Coefficient:
    # Note: this class works only with Fraction number types (this was implemented to prevent floating point errors, which kept cropping up)
    # Convert all incoming number types to Fraction
    # Convert all returned number types to Fraction
    # Internal state should always be stored as Fractions. Internal data checks will return errors if values are accidentally stored as ints or floats
    # NO_SYMBOL = ""
    # OUTPUT_PRECISION = 2
    __terms = {}
    denominator = None
    primaryVariable = None

    def __init__(self, values=Fraction(1), symbolsList=None, denominatorValues=None, denominatorSymbolsList=None, terms=None, denominator=None):
        if terms != None:
            # override standard construction
            self.__terms = copy.deepcopy(terms)
            self.denominator = copy.deepcopy(denominator)
        else:
            # typical construction
            numberValues = values
            if not(isinstance(values, Iterable)):
                if symbolsList == None:
                    numberValues = [values]
                elif isinstance(symbolsList, Iterable) and not(isinstance(symbolsList, str)):
                    if len(symbolsList) == 1:
                        numberValues = [0]
                        numberValues.append(values)
                    else:
                        numberValues = [values]
                else:
                    numberValues = [0]
                    numberValues.append(values)
            else:
                numberValues = [0]
                numberValues.extend(values)

            symbols = [SymbolSet()]
            if symbolsList != None:
                if isinstance(symbolsList, Iterable) and not(isinstance(symbolsList, str)):
                    for s in symbolsList:
                        if isinstance(s, SymbolSet):
                            symbols.append(s)
                        elif isinstance(s, Symbol):
                            symbols.append(SymbolSet(s))
                        else:
                            symbols.append(SymbolSet(s))
                else:
                    if isinstance(symbolsList, SymbolSet):
                        symbols.append(symbolsList)
                    else:
                        symbols.append(SymbolSet(symbolsList))

            # Pad out number values with zeros to match size of keys list, if needed
            for n in range(len(symbols) - len(numberValues)):
                numberValues.append(Fraction(0))

            numberValues = toFraction(numberValues)
            self.__terms = {}

            # Add together values for duplicate symbols
            for i in range(len(symbols)):
                if symbols[i] in self.__terms.keys():
                    self.__terms[symbols[i]] += numberValues[i]
                else:
                    self.__terms[symbols[i]] = numberValues[i]

            

            if denominatorValues != None or denominatorSymbolsList != None:
                self.denominator = Coefficient(denominatorValues, denominatorSymbolsList)

        self.primaryVariable = None
        self.cleanUp()

    def __str__(self):
        return self.toString()

    def __add__(self, other):
        return self.add(other)
    
    def __radd__(self, other):
        return self.__add__(other)

    def __iadd__(self, other):
        return self.add(other, overwriteSelf=True)

    def __sub__(self, other):
        return self.add(other * -1)

    def __rsub__(self, other):
        return self.__sub__(other)

    def __isub__(self, other):
        return self.add(other * -1, overwriteSelf=True)

    def __mul__(self, other):
        return self.multiply(other)
    
    def __rmul__(self, other):
        return self.__mul__(other)

    def __imul__(self, other):
        return self.multiply(other, overwriteSelf=True)

    def __truediv__(self, other):
        return self.divide(other)

    def __rtruediv__(self, other):
        if isinstance(other, (Fraction, int, float)):
            otherCoefficient = Coefficient(other)
            return otherCoefficient / self
        else:
            return NotImplemented

    def __itruediv__(self, other):
        return self.divide(other, overwriteSelf=True)

    def __floordiv__(self, other):
        if self.primaryVariable == None:
            return NotImplemented
        else:
            return self.symbolicDivide(other)[0]

    def __mod__(self, other):
        if isinstance(other, Coefficient):
            # print(self)
            # print(other)
            # time.sleep(1)
            if other.isNumber():
                return self % other.evaluate()
            elif self.primaryVariable == None:
                allVariables = self.getAllVariables()
                allVariables.discard("")
                if len(allVariables) == 1:
                    self.setPrimaryVariable(allVariables.pop())
                    return self.symbolicDivide(other)[1]
                elif len(allVariables) == 0:
                    # we have established other has indeterminate variables, but self does not
                    return self
                return NotImplemented
            else:
                return self.symbolicDivide(other)[1]

        elif isinstance(other, (int, Fraction)):
            if self.hasDenominator():
                return self
            else:
                newTerms = {}
                for s, value in self.getSymbolsAndValues():
                    newTerms[s] = value % other
                return self.arithmeticReturnValue(newTerms, None, False)

        else:
            return NotImplemented

    def __pow__(self, other):
        return self.raiseToPower(other)

    def __eq__(self, other):
        if isinstance(other, Coefficient):
            # ensure we are working with a copy of other's data, to prevent accidental tampering
            other = other.copy()

            # clean up and simplify both self and other to their most basic forms
            self.cleanUp()
            self.simplify()
            other.cleanUp()
            other = other.simplify()

            # note: because of the simplify operation, trivial denominators (equal to 1) have been removed
            if self.hasDenominator():
                if other.hasDenominator():
                    # separately check that numerators are equal to each other and denominators are equal to each other

                    if self.denominator == other.denominator: # recursive call to this same function for denominators
                        numerator1 = self.getNumerator()
                        numerator2 = other.getNumerator()
                        return numerator1 == numerator2 # recursive call to this same function for numerators
                    else:
                        return False
                else:
                    # denominators do not match
                    return False
            else:
                # self does not have a denominator

                # return false if other has a denominator, when self does not
                if other.hasDenominator():
                    return False

                # we have now established that neither self nor other has a 

                keys = self.getNumeratorSymbols()

                # return false if the symbol set for self does not match the symbol set for other
                if keys != other.getNumeratorSymbols():
                    return False

                # check whether the values corresponding to each symbol match
                for key in keys:
                    if self.__terms[key] != other.getTerms()[key]:
                        return False

                # we have established that symbols match and the values for all symbols match
                return True

        elif isinstance(other, (Fraction, int, float)):
            # the types don't match, but check whether they trivially evaluate to the same number
            try:
                return other == self.evaluate()
            except:
                return False
        else:
            return False

    def isNumber(self):
        # returns true iff the expression trivially evaluates to a form with only numbers, no symbols
        for key, value in self.__terms.items():
            if isinstance(value, Coefficient):
                # return false if there are any non-identity symbols with nonzero values in the expression
                if (key.isIdentity):
                    if not(value.isNumber()):
                        return False
                else:
                    if not(value.isZero()):
                        return False
            elif isinstance(value, Fraction):
                # return false if there are any non-identity symbols with nonzero values in the expression
                if not(key.isIdentity() or value == 0):
                    return False
            else:
                raise Exception("coefficient value is not of supported type")
            
        if self.hasDenominator():
            if not(self.denominator.isNumber()): # recursive call to this same function for the denominator
                return False
        
        # return true if all false possibilities have been eliminated
        return True

    def evaluate(self, values=None, symbols=None):
        values = toFraction(values)
        sum = Fraction(0)
        if values == None:
            # no inputs provided, see if expression can trivially evaluate to a number
            for key, value in self.__terms.items():
                if not(key.isIdentity()):
                    raise Exception("no values provided for variables - cannot evaluate")
                if isinstance(value, Coefficient):
                    value = value.evaluate() # recursive call to this same function
                if not(isinstance(value, Fraction)):
                    raise Exception("invalid input")
                sum += value
            return sum
        else:
            # if single value is provided, covert to a list for consistent processing below
            if not(isinstance(values, Iterable)):
                values = [values]

            if symbols == None:
                # no symbols provided, can only evaluate if there is a single indeterminate variable
                variablesList = self.getAllVariables()
                if len(variablesList) > 1:
                    raise Exception("cannot evaluate coefficient - provide symbols list to evaluate coefficients that contain 2 or more variables")
                else:
                    # there is a single indeterminate variable
                    # set this single variable as the primary variable
                    self.setPrimaryVariable(variablesList[0])
                    # convert primary variable to a list for consistent processing below
                    symbols = [self.primaryVariable]
            else:
                # one or more symbols provided
                if not(isinstance(symbols, Iterable) and not(isinstance, symbols, str)):
                    # if single value is provided, covert to a list for consistent processing below
                    symbols = [symbols]
                if not(len(values) == len(symbols)):
                    raise Exception("lengths of input lists must match")

            for key, value in self.__terms.items():
                if isinstance(value, Coefficient):
                    value = value.evaluate(values, symbols) # recursively call this function for nested coefficients
                if not(isinstance(value, Fraction)):
                    raise Exception("invalid input or data structure")
                sum += value * key.evaluate(values, symbols)
        
        if self.hasDenominator():
            sum /= self.denominator.evaluate(values, symbols) # recursively call this function for the denominator

        return sum

    def add(self, other, overwriteSelf=False):
        # create copies of self's data to prevent accidental tampering with original
        newTerms = self.getTerms()
        newDenominator = self.getDenominator()

        if isinstance(other, Coefficient):
            # Ensure that all operations are performed with a copy of other's data, not a reference
            other = other.copy()

            if (self.hasDenominator() or other.hasDenominator()) and not self.denominator == other.denominator:
                # Use cross multiplication
                numerator1 = self.getNumerator()
                numerator2 = other.getNumerator()
                if other.hasDenominator():
                    numerator1 *= other.getDenominator()
                if self.hasDenominator():
                    numerator2 *= self.getDenominator()
                newNumerator = numerator1 + numerator2 # recursive call to this function with just numerators
                newTerms = newNumerator.getTerms()

                if self.hasDenominator():
                    if other.hasDenominator():
                        # both have denominators. Multiply them together
                        newDenominator *= other.getDenominator()
                    else:
                        # only self has a denominator. No change to state
                        pass
                
                elif other.hasDenominator():
                    # only other has a denominator
                    newDenominator = other.denominator

            else:
                # no denominators, or the same denominator for both expressions
                for s, value in other.getSymbolsAndValues():
                    # add other's values to self's new values, symbol by symbol
                    if s in newTerms:
                        newTerms[s] += value
                    else:
                        newTerms[s] = value
            
        elif isinstance(other, (Fraction, int, float)):
            other = toFraction(other)

            if self.hasDenominator():
                # Use cross multiplication
                numerator1 = self.getNumerator()
                numerator2 = self.getDenominator() * other
                newNumerator = numerator1 + numerator2
                newTerms = newNumerator.getTerms()
            else:
                # add the number value to the identity (blank) symbol
                if SymbolSet() in newTerms.keys():
                    newTerms[SymbolSet()] += other
                else:
                    newTerms[SymbolSet()] = other
        else:
            return NotImplemented

        return self.arithmeticReturnValue(newTerms, newDenominator, overwriteSelf)

    def multiply(self, other, overwriteSelf=False):
        if self.isZero():
            return Fraction(0)

        # start with an empty dictionary for newTerms. We will build up the dictionary from scratch
        newTerms = {}
        # create copy of numerator to prevent accidental tampering with original
        newDenominator = self.getDenominator()

        if isinstance(other, Coefficient):
            # Ensure that all operations are performed with a copy of other's data, not a reference
            other = other.copy()
            if other.hasDenominator():

                # Multiply denominators
                if self.hasDenominator():
                    newDenominator *= other.getDenominator()
                else:
                    newDenominator = other.getDenominator()

            # Multiply all combinations of symbols to build up a new terms dictionary
            for symbol_a, value_a in self.getSymbolsAndValues():
                for symbol_b, value_b in other.getSymbolsAndValues():
                    symbol_c = symbol_a * symbol_b
                    # if isinstance(symbol_c, Symbol):
                    #     symbol_c = SymbolSet(symbol_c)
                    if symbol_c in newTerms.keys():
                        newTerms[symbol_c] += value_a * value_b
                    else:
                        newTerms[symbol_c] = value_a * value_b

        elif isinstance(other, (Symbol, SymbolSet)):
            # Multiply all symbols in the original by the new symbol
            for s in self.__terms.keys():
                newSymbol = s * other
                # Assign the old symbol's (pre-multiplication) value to the new (multiplied) symbol
                newTerms[newSymbol] = self.__terms[s]

        elif isinstance(other, (Fraction, int, float)):
            other = toFraction(other)
            # Multiply all values by the number
            for s in self.__terms.keys():
                newTerms[s] = self.__terms[s] * other

        else:
            return NotImplemented
        
        return self.arithmeticReturnValue(newTerms, newDenominator, overwriteSelf)

    def divide(self, other, overwriteSelf=False):
        if self == 0:
            return Fraction(0)

        if isinstance(other, Coefficient):
            # Ensure that all operations are performed with a copy of other's data, not a reference
            other = other.copy()

            # Process trivial cases
            if other == 0:
                raise Exception("Divide by zero error")
            elif other.isUnity():
                return self
            elif other == self:
                return Fraction(1)
            elif other.isNumber():
                return self.multiply(1 / Fraction(other), overwriteSelf=overwriteSelf)
            else:
                # Nontrivial division
                # Multiply self's numerator by other's denominator
                newNumerator = self.getNumerator()
                if other.hasDenominator():
                    newNumerator *= other.getDenominator()

                newDenominator = None
                if self.hasDenominator():
                    # Multiply self's denominator by other's numerator
                    newDenominator = self.getDenominator() * other.getNumerator()
                else:
                    # other's numerator becomes the new denominator
                    newDenominator = other.getNumerator()

                return self.arithmeticReturnValue(newNumerator.getTerms(), newDenominator, overwriteSelf)
                    
        elif isinstance(other, (Fraction, int, float)):
            return self.multiply(1 / Fraction(other), overwriteSelf=overwriteSelf)

        else:
            return NotImplemented

    def raiseToPower(self, other):
        if other == 0:
            return Fraction(1)
        returnCoefficient = self.copy()
        # Note: this function currently only supports integer powers
        # TODO: implement fractional powers
        for i in range(math.floor(other) - 1):
            returnCoefficient *= self
        return returnCoefficient

    def arithmeticReturnValue(self, newTerms, newDenominator, overwriteSelf):
        if overwriteSelf:
            self.__terms = newTerms
            self.denominator = newDenominator
            self.cleanUp()
            return self
        else:
            return Coefficient(terms=newTerms, denominator=newDenominator)

    def copy(self):
        return copy.deepcopy(self)

    def getNumerator(self):
        return Coefficient(terms=copy.deepcopy(self.__terms))

    def getDenominator(self):
        if self.hasDenominator():
            return copy.deepcopy(self.denominator)
        else:
            return None

    def getDenominatorTerms(self):
        if self.hasDenominator():
            return self.denominator.getTerms()
        else:
            return None

    def getTerms(self):
        return copy.deepcopy(self.__terms)

    def getSymbolsAndValues(self):
        return copy.deepcopy(self.__terms).items()

    def getNumeratorSymbols(self):
        return set(self.__terms.keys())

    def toString(self, parentheses=False, leadingOperator=False, suppressNegative=False, returnZero=True, returnUnity=True):
        subParentheses = parentheses
        returnString = ""
        if leadingOperator:
            if self.isNegative():
                returnString += " - "
            else:
                returnString += " + "

        if self.hasDenominator():
            subParentheses = True
            returnString += "["

        returnString += self.numeratorString(parentheses=subParentheses, leadingOperator=leadingOperator, suppressNegative=leadingOperator, returnZero=returnZero, returnUnity=returnUnity)
        
        if self.hasDenominator():
            returnString += "/"
            returnString += self.denominatorString(parentheses=True)
            returnString += "]"

        return returnString

    def numeratorString(self, parentheses=False, leadingOperator=False, suppressNegative=False, returnZero=True, returnUnity=True):
        if self.isZero():
            if returnZero:
                return "0"
            else:
                return ""
        if self.isUnity():
            if returnUnity:
                return "1"
            else:
                return ""

        valueList = []
        minus = []
        symbolList = []
        for symbol, value in self.__terms.items():
            # nested coefficient processing
            if isinstance(value, Coefficient):
                if not(value.isZero()):
                    returnUnity = symbol.isIdentity()
                    valueList.append(value.toString(parentheses=True, leadingOperator=False, suppressNegative=True, returnUnity=returnUnity))
                    minus.append(value.isNegative())
                    symbolList.append(symbol.toString())

            # number processing
            elif value != 0:
                if value != 1 or symbol.isIdentity():
                    valueList.append(abs(value))
                else:
                    valueList.append("")
                minus.append(value < 0)
                symbolList.append(symbol.toString())
        
        returnString = ""

        components = sorted(zip(symbolList, valueList, minus), key = lambda x: x[0])
        
        if parentheses and len(components) > 1:
            returnString += "("
            if leadingOperator and components[0][2]:
                for i in range(len(components)):
                    components[i] = (components[i][0], components[i][1], not components[i][2])

        for i in range(len(components)):
            if i == 0:
                if not(leadingOperator) and components[0][2] and not(suppressNegative):
                    returnString += "-"
            else:
                if components[i][2]:
                    returnString += " - "
                else:
                    returnString += " + "
            
            if isinstance(components[i][1],str):
                returnString += components[i][1]
            else:
                returnString += str(components[i][1])
            returnString += components[i][0]
        
        if parentheses and len(components) > 1:
            returnString += ")"
        
        return returnString

    def denominatorString(self, parentheses=False):
        if self.hasDenominator():
            return self.denominator.toString(parentheses=parentheses)
        else:
            return ""

    def removeZeroTerms(self):
        for key, value in self.getSymbolsAndValues(): # operate on a copy of the dictionary so we can still modify the original dictionary
            if isinstance(value, Coefficient):
                if value.isZero():
                    if not key.isIdentity():
                        del self.__terms[key]
                elif value.isNumber():
                    self.__terms[key] = value.evaluate()
            elif isinstance(value, Fraction):
                if value == 0:
                    if key.isIdentity():
                        self.__terms[key] = Fraction(0)
                    else:
                        del self.__terms[key]
            else:
                raise Exception("Invalid data")

        # Add a zero value to the identity symbol if it somehow got removed
        if len(self.__terms) == 0:
            self.__terms = {SymbolSet(): Fraction(0)}

        if self.isZero():
            self.denominator = None

        if self.hasDenominator():
            self.denominator.removeZeroTerms() # recursively call this function for the denominator

    def cleanUp(self):
        self.removeZeroTerms()
        
        if self.hasDenominator():
            if self.denominator.isZero():
                raise Exception("Divide by zero error")

            # Cancel out any symbols in numerator and denominator where possible
            symbolList = self.getAllSymbols()
            allVariables = self.getAllVariables()

            # create a dictionary of all variables found in both the numerator and denominator
            commonVariables = dict.fromkeys(allVariables, None)

            for variable in allVariables:
                for s in symbolList:
                    if s.variable == variable:
                        # track the lowest exponent that is found for that symbol among all the keys
                        if commonVariables[variable] == None:
                            commonVariables[variable] = s.exponent
                        else:
                            commonVariables[variable] = min(s.exponent, commonVariables[variable])
                    else:
                        # if any key does not contain the symbol, delete it from the commonVariables dictionary
                        del commonVariables[variable]
                        break

            # we are left with a dictionary of the highest exponent symbols that are common to all keys in both the numerator and denominator
            for variable, exponent in commonVariables.items():
                # divide all numerator keys by the common symbol
                newNumeratorTerms = {}
                for key in self.__terms.keys():
                    newNumeratorTerms[key / Symbol(variable, exponent)] = self.__terms[key]
                self.__terms = newNumeratorTerms

                # divide all denominator keys by the common symbol
                newDemoninatorTerms = {}
                for key in self.denominator.__terms.keys():
                    newDemoninatorTerms[key / Symbol(variable, exponent)] = self.denominator.__terms[key]
                self.denominator = Coefficient(terms=newDemoninatorTerms)

            self.denominator.cleanUp() # recursively call this function for the denominator

            if self.denominator.isUnity():
                self.denominator = None

            # if the denominator trivially evaluates to a number, remove the denominator by dividing it into the numerator
            # avoid calling basic Coefficient arithmetic functions to prevent endless recursion
            elif self.denominator.isNumber():
                self.divideNumeratorTermsByConstant(self.denominator.evaluate())
                self.denominator = None

            # reduce all number values by the greatest common divisor
            # avoid calling basic Coefficient arithmetic functions to prevent endless recursion
            if self.hasDenominator():
                self.divideAllTermsByConstant(self.getGcd_Number())

            # TODO: factor numerator and denominator and check if denominator can divide numerator and simplify

        if self.isZero():
            self.denominator = None

        self.setDefaultIndeterminateVariable()
        # # If expression has a single indeterminate variable, set it as the primary variable
        # allVariables = self.getAllVariables()
        # allVariables.discard("")
        # self.primaryVariable = None
        # if len(allVariables) == 1:
        #     self.setPrimaryVariable(allVariables.pop())

    def setDefaultIndeterminateVariable(self):
        # If expression has a single indeterminate variable, set it as the primary variable
        allVariables = self.getAllVariables()
        allVariables.discard("")
        if len(allVariables) == 1:
            self.setPrimaryVariable(allVariables.pop())
        elif len(allVariables) == 0:
            self.primaryVariable = None

    def divideNumeratorTermsByConstant(self, constant):
        for key in self.__terms.keys():
            if isinstance(self.__terms[key], Coefficient):
                self.__terms[key].divideAllTermsByConstant(constant)
            elif isinstance(self.__terms[key], Fraction):
                self.__terms[key] /= constant
            else:
                raise Exception("invalid data structure")

    def divideAllTermsByConstant(self, constant):
        if constant != 1:
            self.divideNumeratorTermsByConstant(constant)

            if self.hasDenominator():
                self.denominator.divideAllTermsByConstant(constant)

    def getNumeratorIndividualSymbols(self):
        symbols = set()
        for key, value in self.__terms.items():
            # eliminate any symbols with zero values
            if not((isinstance(value, Coefficient) and value.isZero()) or (isinstance(value, Fraction) and value == 0)):
                symbols = symbols | key.symbolsSet

                if isinstance(value, Coefficient):
                    symbols = symbols | value.getAllSymbols()
        
        return symbols

    def getAllSymbols(self):
        symbols = self.getNumeratorIndividualSymbols()
        if self.hasDenominator():
            symbols = symbols | self.denominator.getAllSymbols()

        return symbols

    def getNumeratorVariables(self):
        variables = set()
        for s in self.getNumeratorIndividualSymbols():
            # remove identity symbol from variables
            if not(s.isIdentity()):
                variables.add(s.variable)
        return variables

    def getAllVariables(self):
        variables = self.getNumeratorVariables()
        if self.hasDenominator():
            variables = variables | self.denominator.getNumeratorVariables()

        return variables

    def getNumeratorValues(self):
        # return a list of all values in the numerator (no particular order)
        valuesList = []
        for value in self.__terms.values():
            if isinstance(value, Coefficient):
                valuesList = valuesList.extend(value.getAllValues())
            elif isinstance(value, Fraction):
                valuesList.append(value)
            else:
                raise Exception("Invalid data type")
        
        return valuesList

    def getAllValues(self):
        valuesList = self.getNumeratorValues()
        if self.hasDenominator():
            valuesList.extend(self.denominator.getNumeratorValues())

        return valuesList

    def getTerms(self):
        return copy.deepcopy(self.__terms)

    def gcd2Coefficient(self, b, variable = None):
        if variable == None:
            variable = self.primaryVariable

        if variable == None:
            selfDivisor = self.getGcd_Number()
            if isinstance(b, Coefficient):
                b = b.getGcd_Number()
            return self.gcd2Numbers(selfDivisor, b)
        else:
            if isinstance(b, Coefficient):
                a = self.copy()
                while not(a == 0):
                    temp = a
                    a = b % a
                    b = temp
                return b
            else:
                selfDivisor = self.getGcd_Number()
                return self.gcd2Numbers(selfDivisor, b)

    def gcd2Numbers(self, a, b):
        if not(isinstance(a, Fraction) and isinstance(b, Fraction)):
            raise Exception("invalid input")
        
        while not(a == 0):
            temp = a
            a = b % a
            b = temp
        return b

    def getGcd_Number(self):
        values = self.getAllValues()
        
        result = values[0]
        for n in values[1:]:
            result = self.gcd2Numbers(n, result)
        return result

    def smallestNumberTerm(self):
        smallest = None
        for value in self.__terms.values():
            if isinstance(value, Coefficient):
                if smallest == None:
                    smallest = value.smallestNumberTerm()
                else:
                    smallest = min(smallest, value.smallestNumberTerm())
            elif isinstance(value, (int, float)):
                if smallest == None:
                    smallest = value
                else:
                    smallest = min(smallest, value)
        return smallest

    def isZero(self):
        for v in self.__terms.values():
            if isinstance(v, Coefficient):
                if not(v.isZero()):
                    return False
            else:
                if v != 0:
                    return False
        
        return True

    def isUnity(self, allowNegative = True):
        if self.hasDenominator():
            return False
        for symbol, value in self.__terms.items():
            if symbol.isIdentity():
                if isinstance(value, Coefficient) and not(value.isUnity()):
                    return False
                else:
                    try:
                        valueNumber = value.evaluate()
                    except:
                        return False
                    if not(allowNegative) and valueNumber < 0:
                        return False
                    if abs(valueNumber) != 1:
                        return False
            else:     
                if isinstance(value, Coefficient) and not(value.isZero()):
                        return False
                else:
                    if value != 0:
                        return False
        
        return True

    def hasDenominator(self):
        return self.denominator != None

    def isNegative(self):
        if self.isZero():
            return False

        minus = []
        symbolList = []
        for symbol, value in self.__terms.items():
            # nested coefficient processing
            if isinstance(value, Coefficient):
                if not(value.isZero()):
                    minus.append(value.isNegative())
                    symbolList.append(symbol.toString())

            # number processing
            elif value != 0:
                minus.append(value < 0)
                symbolList.append(symbol.toString())

        components = sorted(zip(symbolList, minus), key = lambda x: x[0])
        return components[0][1]

    def convertToPolynomial(self, variable):
        # Converts the numerator to a polynomial, using the specified variable as the indeterminate
        if self.hasDenominator():
            raise Exception("cannot convert coefficient with denominator to polynomial")
        
        polyTerms = {}
        for s, value in self.getSymbolsAndValues():
            exp = s.getExponent(variable)
            if exp != 0:
                s /= Symbol(variable, exp)
            coeff = Coefficient(value, s)
            if exp in polyTerms.keys():
                polyTerms[exp] += coeff
            else:
                polyTerms[exp] = coeff
        
        coefficients = []
        exponents = []
        for e, value in polyTerms.items():
            coefficients.append(value)
            exponents.append(e)

        return PolynomialExpression(coefficients, exponents)

    def symbolicDivide(self, other, variable=None):
        # Euclidian Polynomial Division with respect to given variable
        if not(isinstance(other, Coefficient)):
            raise Exception("invalid type: input must be a Coefficient")

        if other == 0:
            raise Exception("Divide by zero error")

        if variable == None:
            if self.primaryVariable == None:
                raise Exception("Specify indeterminate variable for symbolic division")
            else:
                variable = self.primaryVariable

        A = self.convertToPolynomial(variable)
        B = other.convertToPolynomial(variable)

        Q_R = A.polyDivide(B)

        return([Q_R[0].convertToCoefficient(variable),Q_R[1].convertToCoefficient(variable)])

    def simplify(self, variable=None):
        self.cleanUp()
        if self.hasDenominator():
            if variable == None:
                if self.primaryVariable == None:
                    raise Exception("Specify indeterminate variable for simplification")
                else:
                    variable = self.primaryVariable
            Q_R = self.getNumerator().symbolicDivide(self.denominator, variable)
            if Q_R[1] == 0:
                self.__terms = Q_R[0].__terms
                self.denominator = None
                self.cleanUp()
        
        return self

    def setPrimaryVariable(self, variable):
        self.primaryVariable = variable

class SymbolicMatrix:
    rows = 0
    columns = 0
    data = []
    FLOATING_POINT_ERROR_THRESHOLD = 0.00000000001

    def __init__(self, *rows, rowCount=None, columnCount=None, data=None):
        if data != None:
            # override standard construction
            self.data = data
        else:
            # typical construction
            self.data = []

            if len(rows) == 0:
                if rowCount == None:
                    rowCount = 1
                if columnCount == None:
                    columnCount = 1
                self.rows = rowCount
                self.columns = columnCount
                for r in range(rowCount):
                    self.data.append([0 for i in range(columnCount)])
            else:
                for row in rows:
                    if not(isinstance(row, Iterable)):
                        row = [row]
                    self.data.append(row)

        self.padArray()
        self.fixFloatingPointErrors()

    def __delitem__(self, key):
        del self.data[key]
        self.rows -= 1

    def __getitem__(self, key):
        return self.data[key]

    def __setitem__(self, key, value):
        if isinstance(value, Iterable):
            self.data[key] = value
            self.padArray()

    def __mul__(self, other):
        return self.multiply(other)

    def __rmul__(self, other):
        return self.multiply(other, reverseOrder=True)

    def __str__(self):
        return self.toString()

    def toString(self):
        longestStringByCol = []
        for r in range(self.rows):
            for c in range(self.columns):
                if c + 1 > len(longestStringByCol):
                    longestStringByCol.append(0)
                longestStringByCol[c] = max(longestStringByCol[c], len(str(self.data[r][c])))
        
        tabsByCol = []
        for c in range(self.columns):
            if c + 1 > len(tabsByCol):
                    tabsByCol.append(0)
            tabsByCol[c] = 1 + (longestStringByCol[c] + 2) // 8

        returnString = ""
        for r in range(self.rows):
            rowString = ""
            if r == 0:
                rowString += "["
            else:
                rowString += " "
            for c in range(self.columns):
                dataString = str(self.data[r][c])
                rowString += dataString
                if c < (self.columns - 1):
                    tabs = tabsByCol[c] - (len(dataString)) // 8
                    for tab in range(tabs):
                        rowString += "\t"
            if r == (self.rows - 1):
                rowString += "]"
            else:
                rowString += "\n"
            returnString += rowString
        return returnString
        
    def padArray(self):
        rowCheck = 0
        colCheck = 0

        for row in self.data:
            rowCheck += 1
            colCheck = max(colCheck, len(row))
        
        self.rows = rowCheck
        self.columns = colCheck

        for i in range(self.rows):
            self.data[i] = self.data[i] + [0]*(self.columns - len(self.data[i]))

    def deleteRows(self, rowsToDelete):
        if not(isinstance(rowsToDelete, Iterable)):
            rowsToDelete = [rowsToDelete]
        
        shift = 0
        for row in sorted(rowsToDelete):
            self.__delitem__(row - shift)
            shift += 1

    def __deleteColumn__(self, column):
        for row in self.data:
            del row[column]
        
        self.columns -= 1

    def deleteColumns(self, columnsToDelete):
        if not(isinstance(columnsToDelete, Iterable)):
            columnsToDelete = [columnsToDelete]
        
        shift = 0
        for col in sorted(columnsToDelete):
            self.__deleteColumn__(col - shift)
            shift += 1

    def submatrix(self, rows, cols):
        newRows = [self.data[i] for i in rows]
        for i in range(len(newRows)):
            newRows[i] = [newRows[i][c] for c in cols]
        
        return SymbolicMatrix(data=newRows)

    def det(self):
        if not(self.rows == self.columns):
            raise Exception("matrix is not square - cannot calculate determinant")
        else:
            # Calculate determinant using minor expansion algorithm
            # M stores the determinants of submatrices
            # Note: the keys for M are frozen sets representing the columns of submatrices (the rows of the submatrix are the first n rows, where n is the size of the set)
            M = {}
         
            # set determinants of size 1 submatrices for the first row (i is the column)
            # the determinant of a size 1 submatrix is itself
            for i in range(0,self.rows):
                M[frozenset([i])] = self.data[0][i]

            # calculate determinant of submatrices of increasing size, using previously-calculated determinants for smaller submatrices
            for i in range(1,self.rows):
                columnCombinations = combinationsFrozen(range(self.rows), i+1)
                for J in columnCombinations:
                    M[J] = 0
                    cols = sorted(list(J)) # unfreeze and sort combination
                    for k in range(i+1):
                        subcombination = cols.copy()
                        jk = subcombination.pop(k)
                        J_lessjk = frozenset(subcombination)
                        M[J] += (-1) ** (i + k) * self.data[i][jk] * M[J_lessjk]
            
            # return the determinant calculated for the full matrix
            n = frozenset(range(self.rows))
            return M[n]

    def inverse(self):
        augmentedMatrix = SymbolicMatrix(data=self.data)
        for r in range(augmentedMatrix.rows):
            for c in range(augmentedMatrix.rows):
                augmentedMatrix.data[r].append(1 * (r == c))

        augmentedMatrix.columns += augmentedMatrix.rows
        processedMatrix = augmentedMatrix.gaussJordanElimination()
        processedMatrix.deleteColumns(range(augmentedMatrix.rows))
        return processedMatrix

    def swapRows(self, r1, r2):
        temp = self.data[r1]
        self.data[r1] = self.data[r2]
        self.data[r2] = temp

    def divideRow(self, row, divisor):
        self.data[row] = [c / divisor for c in self.data[row]]

    def subtractRowMultiple(self, targetRow, operatorRow, multiple):
        m = copy.deepcopy(multiple)
        for c in range(self.columns):
            if self.data[operatorRow][c] != 0:
                self.data[targetRow][c] -= self.data[operatorRow][c] * m

    def gaussJordanElimination(self):
        M = SymbolicMatrix(data=self.data.copy())
        k = 0
        l = -1
        for k in range(M.rows):
            while l <= M.columns:
                l += 1
                for i in range(k, M.rows):
                    if M.data[i][l] != 0:
                        break
                else:
                    continue
                break

            if M.data[k][l] == 0 and not(k == M.rows):
                for i in range(k+1, M.rows):
                    if M.data[i][l] != 0:
                        M.swapRows(i,k)
                        break

            M.divideRow(k, M.data[k][l])

            for i in range(M.rows):
                if i != k:
                    M.subtractRowMultiple(i,k,M.data[i][l])

        return M
            
    def multiply(self, other, reverseOrder=False):
        newData = []
        if isinstance(other, SymbolicMatrix):
            A = self
            B = other
            if reverseOrder:
                B = self
                A = other

            if A.columns != B.rows:
                raise Exception("incompatible arrays")
            
            for r in range(A.rows):
                newRow = []
                for c in range(B.columns):
                    sum = 0
                    for r2 in range(B.rows):
                        sum += A.data[r][r2] * B.data[r2][c]
                    newRow.append(sum)
                newData.append(newRow)

        elif isinstance(other, (Coefficient, int, float)):
            newData = self.data.copy()
            for r in range(self.rows):
                for c in range(self.columns):
                    newData[r][c] *= other

        else:
            return NotImplemented
            
        return SymbolicMatrix(data=newData)

    def fixFloatingPointErrors(self):
        SIGNIFICANT_FIGURES = 6
        for r in range(self.rows):
            for c in range(self.columns):
                if isinstance(self.data[r][c], float):
                    if abs(self.data[r][c]) < self.FLOATING_POINT_ERROR_THRESHOLD:
                        self.data[r][c] = 0
                    else:
                        roundPosition = SIGNIFICANT_FIGURES - (math.floor(math.log10(abs(self.data[r][c]))) + 1)
                        self.data[r][c] = round(self.data[r][c], roundPosition)

    def solve(self, B):
        return self.inverse() * B

    def solveToList(self, B):
        matrixSolution = self.solve(B)
        return matrixSolution.transpose()[0]

    def transpose(self, inPlace=False):
        newData = []
        for c in range(self.columns):
            newRow = []
            for r in range(self.rows):
                newRow.append(self.data[r][c])
            newData.append(newRow)
        if inPlace:
            self.data = newData
            rows = self.rows
            columns = self.columns
            self.rows = columns
            self.columns = rows
            return self
        else:
            return SymbolicMatrix(data = newData)

class IrrationalRoot:
    value = 0
    exponent = 0
    negative = False

    def __init__(self, value, exponent, negative=False):
        self.value = toFraction(value)
        if exponent < 0:
            self.value = 1 / self.value
        self.exponent = toFraction(abs(exponent))
        self.negative = negative

    def __str__(self):
        return self.toString()

    def __add__(self, other):
        return self.add(other)

    def __radd__(self, other):
        return self.add(other)

    def __sub__(self, other):
        return self.add(other * -1)

    def __rsub__(self, other):
        negativeSelf = self * -1
        return negativeSelf.add(other)

    def __mul__(self, other):
        return self.multiply(other)

    def __rmul__(self, other):
        return self.multiply(other)
    
    def __pow__(self, other):
        return self.raiseToPower(other)

    def __abs__(self):
        return IrrationalRoot(self.value, self.exponent)

    def __eq__(self, other):
        selfValue = self.evaluate()
        if isinstance(selfValue, Fraction):
            return selfValue == other
        else:
            if isinstance(other, IrrationalRoot):
                return self.value == other.value and self.exponent == other.exponent and self.negative == other.negative
                #TODO: check if a simplification can make them equal (for example, 4^1/2 = 16^1/4)
            else:
                return False

    @property
    def sign(self):
        if self.negative:
            return -1
        else:
            return 1

    def add(self, other):
        selfValue = self.evaluate()
        if isinstance(selfValue, Fraction):
            return selfValue + other
        else:
            # TODO: return number
            return NotImplemented

    def multiply(self, other):
        if isinstance(other, IrrationalRoot):
            other = other.evaluate()

        if isinstance(other, IrrationalRoot):
            selfValue = self.value ** (self.exponent.numerator * other.exponent.denominator)
            otherValue = other.value ** (other.exponent.numerator * self.exponent.denominator)
            newValue = selfValue * otherValue
            newExponent = Fraction(1, self.exponent.denominator * other.exponent.denominator)
            return IrrationalRoot(newValue, newExponent, self.negative != other.negative).evaluate()
        else:
            other = toFraction(other)
            if isinstance(other, Fraction):
                newValue = self.value * abs(other) ** (1 / self.exponent)
                negative = self.negative != (other < 0)
                return IrrationalRoot(newValue, self.exponent, negative).evaluate()
            else:
                return NotImplemented

    def divide(self, other):
        if isinstance(other, IrrationalRoot):
            other = other.evaluate()

        if isinstance(other, IrrationalRoot):
            selfValue = self.value ** (self.exponent.numerator * other.exponent.denominator)
            otherValue = other.value ** (other.exponent.numerator * self.exponent.denominator)
            newValue = selfValue / otherValue
            newExponent = Fraction(1, self.exponent.denominator * other.exponent.denominator)
            return IrrationalRoot(newValue, newExponent, self.negative != other.negative).evaluate()
        else:
            other = toFraction(other)
            if isinstance(other, Fraction):
                newValue = self.value / abs(other) ** (1 / self.exponent)
                negative = self.negative != (other < 0)
                return IrrationalRoot(newValue, self.exponent, negative).evaluate()
            else:
                return NotImplemented

    def raiseToPower(self, other):
        other = toFraction(other)
        if isinstance(other, Fraction):
            newNumerator = self.exponent.numerator * other.numerator
            newDenominator = self.exponent.denominator * other.denominator
            return IrrationalRoot(self.value, Fraction(newNumerator, newDenominator)).evaluate()
        else:
            return NotImplemented

    def evaluateCoefficient(self):
        # attempt to simplify the coefficient to a rational number (result may still be complex)
        n = abs(self.value).numerator
        d = abs(self.value).denominator

        nRoot = integerRoot(n, self.exponent)
        dRoot = integerRoot(d, self.exponent)

        if nRoot != None and dRoot != None:
            return Fraction(nRoot, dRoot)
        else:
            return IrrationalRoot(abs(self.value), self.exponent)

    def evaluate(self):
        # attempt to simplify the result to a rational number
        if self.exponent >= 1:
            return self.sign * self.value ** self.exponent
        
        if self.value < 0:
            return IrrationalRoot(self.value, self.exponent, self.negative)
        else:
            return self.evaluateCoefficient() * self.sign

    def isNegative(self):
        selfValue = self.evaluate()
        if isinstance(selfValue, Fraction):
            if self.negative:
                selfValue *= -1
            return selfValue < 0
        else:
            return self.negative

    def toString(self):
        simplified = self.evaluate()
        if isinstance(simplified, IrrationalRoot):
            returnString = ""
            if self.negative:
                returnString += "-"
            coeff = self.evaluateCoefficient()
            if isinstance(coeff, Fraction):
                # we know the result is a complex number, because the whole expression could not be simplified
                returnString += str(coeff) + "i"
            else:
                if self.value < 0:
                    returnString += "(" + str(self.value) + ")^" + str(self.exponent)
                else:
                    returnString += str(self.value) + "^" + str(self.exponent)

            return returnString
        else:
            if self.negative:
                simplified *= -1
            return str(simplified)

    def isComplex(self):
        return self.value < 0 and self.exponent < 1

# t = Symbol("t")
# b = Symbol("t")

# A = SymbolSet([b, t])
# B = SymbolSet([t, b])

# for e in A.symbolsSet:
#     print(e)
# print("---------------------")
# for e in B.symbolsSet:
#     print(e)
# print("---------------------")
# print(A.symbolsSet)
# print(B.symbolsSet)
# print(A == B)


# A = Coefficient([49,-8484,477680,-8030400,-40320000],[Symbol(),Symbol("t",2),Symbol("t",4),Symbol("t",6),Symbol("t",8)])
# B = Coefficient([-6,720,-21600],[Symbol("t",1),Symbol("t",3),Symbol("t",5)])

# print(A)
# print(B)
# print("-----------------------")

# C = A / B
# print(C)

# print(A // B)

# C = A/B
# print(C)

# A = PolynomialExpression([3,1,1,5],[3,2,1,0])

# B = PolynomialExpression([5,-3,1],[2,1,0])

# # print(A + A / B)

# print(A // B)