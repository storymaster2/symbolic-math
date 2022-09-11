from abc import ABC, abstractmethod
import fractions
import copy
import math
from collections.abc import Iterable
import decimal
import time

# helper functions

def gcd(a, b):
    a = abs(a)
    b = abs(b)
    while not(a == 0):
        temp = a
        a = b % a
        b = temp
    return b

def integerRoot(value, root):
    if not(isinstance(value, int)):
        raise ValueError("value must be of type int")
    if not(isinstance(root, int)):
        raise ValueError("root must be of type int")
    if root == 1:
        return value

    n = 1
    while n ** root <= value:
        if n ** root == value:
            return n
        n += 1
    
    return None

def toRational(a):
    if isinstance(a, (float, decimal, fractions.Fraction, int)):
        return Rational(a)
    else:
        return a

def roundFloatForDisplay(x):
    leadingZeroes = -math.ceil(math.log10(abs(x)))
    roundingPrecision = 2
    if leadingZeroes > 0:
        roundingPrecision += leadingZeroes
    return round(x, roundingPrecision)

# end helper functions
# ------------------------------------------
# ------------------------------------------
# ------------------------------------------
# ------------------------------------------

# parent interface for all terms
class Termlike(ABC):
    # supported types
    TYPE_RATIONAL = "rational"
    TYPE_ROOT = "root"
    TYPE_IRRATIONAL = "irrational"
    TYPE_SYMBOL = "symbol"
    TYPE_IMAGINARY = "imaginary"
    TYPE_FRACTION = "fraction"
    TYPE_TERM = "term"
    TYPE_EXPRESSION = "expression"

    # supported organization options
    ORGANIZATION_TYPE_POLYNOMIAL = "polynomial"
    ORGANIZATION_TYPE_SEPARATE_TERMS = "separate terms"
    ORGANIZATION_TYPE_DO_NOT_SIMPLIFY = "do not simplify"
    validOrganizationTypes = [
        ORGANIZATION_TYPE_POLYNOMIAL,
        ORGANIZATION_TYPE_SEPARATE_TERMS,
        ORGANIZATION_TYPE_DO_NOT_SIMPLIFY
    ]
    extractConstant = False
    organization = ORGANIZATION_TYPE_POLYNOMIAL
    primaryIndeterminate = "x"

    def __init__(self):
        pass

    # Overrides and helper functions (do not override in subclasses)
    def __str__(self):
        self.simplify()
        return self.toString()

    def __add__(self, other):
        other = self.cleanInput(other)
        if self.__class__ == other.__class__:
            return self.add(other).simplify()
        else:
            # return an Expression
            return Expression(self, other)

    def __radd__(self, other):
        return self.__add__(other)
    
    def __iadd__(self, other):
        other = self.cleanInput(other)

        if self.__class__ == other.__class__:
            return self.add(other, overwriteSelf=True).simplify()
        else:
            # convert self to an Expression
            self.convertTo(Expression(self, other))
            return self

    def __sub__(self, other):
        other = self.cleanInput(other)
        if self == other:
            return Rational(0)
        return self.__add__(other * -1)

    def __rsub__(self, other):
        other = self.cleanInput(other)
        if self == other:
            return Rational(0)
        return (self * -1).__add__(other)

    def __isub__(self, other):
        other = self.cleanInput(other)
        if self == other:
            self.convertTo(Rational(0))
            return self
        return self.__iadd__(other * -1)

    def __mul__(self, other):
        other = self.cleanInput(other)
        if self.__class__ == other.__class__:
            return self.multiply(other).simplify()
        else:
            # return a Term
            return Term(self, other)

    def __rmul__(self, other):
        return self.__mul__(other)

    def __imul__(self, other):
        other = self.cleanInput(other)
        if self.__class__ == other.__class__:
            return self.multiply(other, overwriteSelf=True).simplify()
        else:
            # convert self to a Term
            self.convertTo(Term(self, other))
            return self

    def __truediv__(self, other):
        other = self.cleanInput(other)
        if self == other:
            return Rational(1)
        if self.__class__ == other.__class__:
            return self.divide(other).simplify()
        else:
            # return a Fraction
            return Fraction(self, other)

    def __rtruediv__(self, other):
        other = self.cleanInput(other)
        if self == other:
            return Rational(1)
        if self.__class__ == other.__class__:
            return (other / self).copy()
        else:
            inverse = Fraction(1, self)
            returnValue = (inverse * other)
            returnValue.simplify()
            return returnValue

    def __itruediv__(self, other):
        other = self.cleanInput(other)
        if self == other:
            self.convertTo(Rational(1))
            return self
        if self.__class__ == other.__class__:
            return self.divide(other, overwriteSelf=True).simplify()
        else:
            # convert self to a Fraction
            self.convertTo(Fraction(self, other))
            return self

    def __floordiv__(self, other):
        other = self.cleanInput(other)
        if self == other:
            return 1
        if self.isNumeric() and other.isNumeric() and self.__class__ != other.__class__:
            selfNumericValue = self.evaluate(decimalType=True)
            otherNumericValue = other.evaluate(decimalType=True)
            return selfNumericValue // otherNumericValue
        return self.intDivide(other)

    def __mod__(self, other):
        other = self.cleanInput(other)
        if self == other:
            return 0
        if self.isNumeric() and other.isNumeric() and self.__class__ != other.__class__:
            selfNumericValue = self.evaluate(decimalType=True)
            otherNumericValue = other.evaluate(decimalType=True)
            remainder = self - (selfNumericValue // otherNumericValue) * (self / other)
            return remainder.simplify()
        return self.mod(other).simplify()

    def __pow__(self, other):
        other = toRational(other)
        if not(isinstance(other, Rational)):
            raise ValueError("unsupported input type for power function. Use int or number-like type (float, fractions.Fraction, Rational)")

        power = other.numerator
        root = other.denominator

        if power == 0:
            return Rational(1)
        elif other < 0: #negative power
            inverse = Fraction(1, self)
            return inverse.power(power).toRoot(root).simplify()
        else:
            return self.power(power).toRoot(root).simplify()

    def __eq__(self, other):
        if other == None:
            return False
        other = self.cleanInput(other)
        other.simplify()
        self.simplify()

        if self.__class__ == other.__class__:
            return self.equals(other)
        else:
            return False

    def __lt__(self, other):
        if other == None:
            return False
        other = self.cleanInput(other)
        other.simplify()
        self.simplify()

        return self.evaluate() < other.evaluate()

    def __le__(self, other):
        return self.__eq__(other) or self.__lt__(other)

    def __gt__(self, other):
        if other == None:
            return False
        other = self.cleanInput(other)
        other.simplify()
        self.simplify()

        return self.evaluate() > other.evaluate()

    def __ge__(self, other):
        return self.__eq__(other) or self.__gt__(other)

    def __abs__(self):
        return self.abs()

    def cleanInput(self, other):
        # copy input to prevent accidentally modifying original object
        other = copy.deepcopy(other)

        # check for valid input
        if not(isinstance(other, Termlike)):
            other = toRational(other)
            if not(isinstance(other, Rational)):
                raise ValueError("unsupported input type for Termlike arithmetic")
        
        return other

    def convertTo(self, newObject):
        # converts self to the provided object, including changing self's class, data structure, and methods
        self.__class__ = newObject.__class__
        self.__dict__ = newObject.__dict__
        self.simplify()

    def copy(self):
        return copy.deepcopy(self)

    def evaluate(self, values=None, variables=None, inplace=False, decimalType=False):
        values = copy.deepcopy(values)
        variables = copy.deepcopy(variables)
        numericResult = True

        if variables == None:
            if values == None:
                return self.evaluateToNumber(decimalType=decimalType)
            elif isinstance(values, Iterable):
                raise ValueError("values list provided without matching variables list. Cannot assign values to variables")
            else:
                values = toRational(values)
                if not(isinstance(values, Termlike)):
                    raise ValueError("invalid type: evaluation value must be of type Termlike or a number type: int, Rational, fractions.Fraction, or float")
                if not(isinstance(values, Rational)):
                    numericResult = False

        else:
            if not(isinstance(variables, Iterable)):
                variables = [variables]
            if not(isinstance(values, Iterable)):
                values = [values]
            if not(len(variables) == len(values)):
                raise ValueError("lengths of input lists do not match")
            for s in variables:
                if not(isinstance(s, str)):
                    raise ValueError("invalid type: variables list shall contain only strings")

            for v in values:
                v = toRational(v)
                if not(isinstance(v, Termlike)):
                    raise ValueError("invalid type: evaluation values must be of type Termlike or a number type: int, Rational, fractions.Fraction, or float")
                if not(isinstance(v, Rational)):
                    numericResult = False
        
        if numericResult and not inplace:
            return self.evaluateToNumber(values=values, variables=variables, decimalType=decimalType)
        else:
            return self.evaluateToTerm(values=values, variables=variables, inplace=inplace)

    def setOrganization(self, organizationCode):
        if organizationCode in self.validOrganizationTypes:
            self.organization = organizationCode
            self.simplify()
        else:
            raise ValueError("organization code provided is not valid")

    def setExtractConstant(self, extractConstantValue):
        if isinstance(extractConstantValue, bool):
            self.extractConstant = extractConstantValue
            self.simplify()
        else:
            raise ValueError("extractConstantValue is not of expected type. Provide boolean value")

    def setPrimaryIndeterminate(self, variable):
        if not(isinstance(variable, str)):
            raise ValueError("variable shall be a string")
        if len(variable) != 1:
            raise ValueError("variable string shall be a single character long")
        
        self.primaryIndeterminate = variable
        self.simplify()

    def isConstant(self, withRespectTo):
        # check for valid input and convert to list
        if isinstance(withRespectTo, str):
            if len(withRespectTo != 1):
                raise ValueError("\"with respect to\" variable must be a single character")
            withRespectTo = [withRespectTo]
        elif isinstance(withRespectTo, Iterable):
            for v in withRespectTo:
                if not(isinstance(v, str)):
                    raise ValueError("\"with respect to\" variables must be of type string")
                if len(v != 1):
                    raise ValueError("\"with respect to\" variable must be a single character")
        else:
            raise ValueError("invalid input. String or list expected")

        return self.checkIsConstant(withRespectTo)

    def getConstantTerm(self, withRespectTo):
        # check for valid input and convert to list
        if isinstance(withRespectTo, str):
            if len(withRespectTo != 1):
                raise ValueError("\"with respect to\" variable must be a single character")
            withRespectTo = [withRespectTo]
        elif isinstance(withRespectTo, Iterable):
            for v in withRespectTo:
                if not(isinstance(v, str)):
                    raise ValueError("\"with respect to\" variables must be of type string")
                if len(v != 1):
                    raise ValueError("\"with respect to\" variable must be a single character")
        else:
            raise ValueError("invalid input. String or list expected")

        if self.checkIsConstant(withRespectTo):
            return copy.deepcopy(self)
        else:
            return self.getGreatestCommonConstant(withRespectTo)

    def factorConstant(self, withRespectTo):
        # check for valid input and convert to list
        if isinstance(withRespectTo, str):
            if len(withRespectTo != 1):
                raise ValueError("\"with respect to\" variable must be a single character")
            withRespectTo = [withRespectTo]
        elif isinstance(withRespectTo, Iterable):
            for v in withRespectTo:
                if not(isinstance(v, str)):
                    raise ValueError("\"with respect to\" variables must be of type string")
                if len(v != 1):
                    raise ValueError("\"with respect to\" variable must be a single character")
        else:
            raise ValueError("invalid input. String or list expected")

        if not(self.checkIsConstant(withRespectTo)):
            self.convertTo(Term(self.getGreatestCommonConstant(withRespectTo), self.getNonConstantFactor(withRespectTo)))

    # Optional overrides
    def abs(self):
        # some child objects will override this method, but it is not required
        return self

    def checkIsConstant(self, variableList):
        return True

    def getGreatestCommonConstant(self, variableList):
        return Rational(1)

    def getNonConstantFactor(self, variableList):
        if self.checkIsConstant(variableList):
            return None
        else:
            return self.copy()

    def getTerms(self):
        return copy.deepcopy(self)

    def getTermsByType(self):
        typeCode = ""
        if isinstance(self, Rational):
            typeCode = Termlike.TERM_RATIONAL
        if isinstance(self, Root):
            typeCode = Termlike.TERM_ROOT
        if isinstance(self, Irrational):
            typeCode = Termlike.TERM_IRRATIONAL
        if isinstance(self, Symbol):
            typeCode = Termlike.TERM_SYMBOL
        if isinstance(self, Imaginary):
            typeCode = Termlike.TERM_IMAGINARY
        if isinstance(self, Fraction):
            typeCode = Termlike.TERM_FRACTION
        if isinstance(self, Term):
            typeCode = Termlike.TERM_TERM
        if isinstance(self, Expression):
            typeCode = Termlike.TERM_EXPRESSION
        
        termsDict = {}
        termsDict[typeCode] = self.copy()
        return termsDict

    def isNumeric(self):
        for category in self.getTermsByType().keys():
            if (category != Termlike.TYPE_RATIONAL) and (category != Termlike.TYPE_ROOT) and (category != Termlike.TYPE_IRRATIONAL):
                return False

        return True

    # Mandatory overrides
    @abstractmethod
    def toString(self):
        pass

    @abstractmethod
    def equals(self, other):
        pass

    @abstractmethod
    def add(self, other, overwriteSelf=False):
        pass

    @abstractmethod
    def multiply(self, other, overwriteSelf=False):
        pass

    @abstractmethod
    def divide(self, other, overwriteSelf=False):
        pass

    @abstractmethod
    def intDivide(self, other):
        pass

    @abstractmethod
    def mod(self, other):
        pass

    @abstractmethod
    def power(self, other):
        pass

    @abstractmethod
    def toRoot(self, other):
        pass

    @abstractmethod
    def simplify(self):
        pass
 
    @abstractmethod
    def evaluateToTerm(self, values=None, variables=None, inplace=False):
        pass

    @abstractmethod
    def evaluateToNumber(self, values=None, variables=None, decimalType=False):
        pass

    @abstractmethod
    def evaluateNumericalCoefficient(self):
        pass

    @abstractmethod
    def isConstant(self, withRespectTo):
        pass

# primitives
class Rational(Termlike):
    numerator = 0
    denominator = 1

    def __init__(self, numerator=0, denominator=1):
        numerator = copy.deepcopy(numerator)
        denominator = copy.deepcopy(denominator)
        if not(isinstance(numerator, int)):
            if isinstance(numerator, (Rational, fractions.Fraction)):
                denominator *= numerator.denominator
                numerator = numerator.numerator
            elif isinstance(numerator, (float, decimal)):
                leadingZeroes = -math.ceil(math.log10(abs(numerator)))
                multiplier = 1000
                if leadingZeroes > 0:
                    multiplier = 1000 * 10 ** leadingZeroes
                numerator = int(numerator * multiplier)
                denominator *= multiplier
            else:
                raise ValueError("unsupported input type")
        if not(isinstance(denominator, int)):
            if isinstance(denominator, (Rational, fractions.Fraction)):
                numerator *= denominator.denominator
                denominator = denominator.numerator
            elif isinstance(denominator, (float, decimal)):
                leadingZeroes = -math.ceil(math.log10(abs(denominator)))
                multiplier = 1000
                if leadingZeroes > 0:
                    multiplier = 1000 * 10 ** leadingZeroes
                denominator = int(denominator * multiplier)
                numerator *= multiplier
            else:
                raise ValueError("unsupported input type")
        self.numerator = numerator
        self.denominator = denominator
        self.simplify()

    def toString(self):
        if self.numerator == 0:
            return "0"
        if self.denominator == 1:
            return "{numerator}".format(numerator=self.numerator)
        else:
            return "{numerator}/{denominator}".format(numerator=self.numerator, denominator=self.denominator)

    def equals(self, other):
        self.simplify()
        if isinstance(other, Rational):
            other.simplify()
            return self.numerator == other.numerator and self.denominator == other.denominator
        else:
            return False

    def arithmeticReturn(self, numerator, denominator, overwiteSelf):
        if overwiteSelf:
            self.numerator = copy.deepcopy(numerator)
            self.denominator = copy.deepcopy(denominator)
            self.simplify()
            return self
        else:
            return Rational(numerator, denominator)

    def add(self, other, overwriteSelf=False):
        if isinstance(other, Rational):
            # compatible type. Return Rational
            # use cross multiplication
            newNumerator = self.numerator * other.denominator + other.numerator * self.denominator
            newDenominator = self.denominator * other.denominator
            return self.arithmeticReturn(newNumerator, newDenominator, overwriteSelf)
        elif isinstance(other, Termlike):
            # incompatible termlike type, Return Expression
            if overwriteSelf:
                self.convertTo(Expression(self,other))
                return self
            else:
                return Expression(self, other)
        else:
            # incompatible type. Raise Exception
            raise ValueError("unexpected value type. Can only perform arithmetic operations on elements of Termlike type")

    def multiply(self, other, overwriteSelf=False):
        if isinstance(other, Rational):
            # compatible type. Return Rational
            newNumerator = self.numerator * other.numerator
            newDenominator = self.denominator * other.denominator
            return self.arithmeticReturn(newNumerator, newDenominator, overwriteSelf)
        elif isinstance(other, Termlike):
            # incompatible termlike type, Return Term
            if overwriteSelf:
                self.convertTo(Term(self, other))
                return self
            else:
                return Term([self, other])
        else:
            # incompatible type. Raise Exception
            raise ValueError("unexpected value type. Can only perform arithmetic operations on elements of Termlike type")

    def divide(self, other, overwriteSelf=False):
        if isinstance(other, Rational):
            # compatible type. Return Rational
            newNumerator = self.numerator * other.denominator
            newDenominator = self.denominator * other.numerator
            return self.arithmeticReturn(newNumerator, newDenominator, overwriteSelf)
        elif isinstance(other, Termlike):
            # incompatible termlike type, Return Fraction
            if overwriteSelf:
                self.convertTo(Fraction(self, other))
            else:
                return Fraction(self, other)
        else:
            # incompatible type. Raise Exception
            raise ValueError("unexpected value type. Can only perform arithmetic operations on elements of Termlike type")

    def intDivide(self, other):
        # This method is only called if other is not numeric
        return 0

    def mod(self, other):
        # This method is only called if other is not numeric
        return self.copy()

    def power(self, power):
        return Rational(self.numerator ** power, self.denominator ** power)

    def toRoot(self, root):
        if root == 1:
            return self
        
        newNumerator = integerRoot(self.numerator, root)
        newDenominator = integerRoot(self.denominator, root)

        if newNumerator != None and newDenominator != None:
            return Rational(newNumerator, newDenominator)
        else:
            return Root(self, root)

    def abs(self):
        return Rational(abs(self.numerator), abs(self.denominator))

    def simplify(self):
        commonDivisor = gcd(self.numerator, self.denominator)
        self.numerator = abs(self.numerator) // commonDivisor * (1 - 2 * ((self.numerator < 0) != (self.denominator < 0)))
        self.denominator = abs(self.denominator) // commonDivisor

        return self
    
    def evaluateToTerm(self, values=None, variables=None, inplace=False):
        return self
    
    def evaluateToNumber(self, values=None, variables=None, decimalType=False):
        if decimalType:
            return decimal(self.numerator) / decimal(self.denominator)
        return self.numerator / self.denominator

    def evaluateNumericalCoefficient(self):
        return self.evaluateToNumber()

    def isConstant(self, withRespectTo):
        return True

    def getConstantTerm(self):
        return copy.deepcopy(self)

class Root(Termlike):
    radicand = 0
    root = 2

    def __init__(self, radicand=Rational(0), root=2):
        radicand = toRational(radicand)
        if not(isinstance(radicand, Termlike)):
            raise ValueError("radicant is not of supported type")
        if not(isinstance(root, int)):
            raise ValueError("root must be of type int")
        self.radicand = copy.deepcopy(radicand)
        self.root = root
        self.simplify()

    def toString(self):
        if self.radicand == 0:
            return "0"
        else:
            return "({radicand})^(1/{root})".format(radicand=self.radicand, root=self.root)

    def equals(self, other):
        self.simplify()
        # if the simplification reduced self to something else, call the new equals method for the new form
        if not(isinstance(self, Root)):
            return self.equals(other)

        if isinstance(other, Termlike):
            other.simplify()
            if isinstance(other, Root):
                return self.radicand == other.radicand and self.root == other.root
            else:
                return False
        else:
            return False

    def arithmeticReturn(self, radicand, root, overwiteSelf):
        if overwiteSelf:
            self.radicand = copy.deepcopy(radicand)
            self.root = root
            self.simplify()
            return self
        else:
            return Root(radicand, root)

    def add(self, other, overwriteSelf=False):
        if isinstance(other, Termlike):
            # create a separate copy of other to manipulate
            otherAsRoot = copy.deepcopy(other)

            if isinstance(otherAsRoot, Rational):
                otherAsRoot = Root(otherAsRoot ** self.root, self.root)

            if isinstance(otherAsRoot, Root) and self.root == otherAsRoot.root:
                # potentially compatible type. Check whether the terms can be combined
                if self.radicand == otherAsRoot.radicand:
                    return self.arithmeticReturn(self.radicand * 2 ** self.root, self.root, overwriteSelf)

                compatibilityFactor = Rational(otherAsRoot.radicand, self.radicand).toRoot(self.root)
                if isinstance(compatibilityFactor, Rational):
                    return self.arithmeticReturn(self.radicand * (1 + compatibilityFactor) ** self.root, self.root, overwriteSelf)

            # cannot simplify. Return Expression
            if overwriteSelf:
                self.convertTo(Expression(self, other))
                return self
            else:
                return Expression(self, other)
        else:
            raise ValueError("unexpected value type. Can only perform arithmetic operations on elements of Termlike type")

    def multiply(self, other, overwriteSelf=False):
        if isinstance(other, Rational):
            return self.arithmeticReturn(self.radicand * other ** self.root, self.root, overwriteSelf)

        elif isinstance(other, Root):
            if self.root == other.root:
                return self.arithmeticReturn(self.radicand * other.radicand, self.root, overwriteSelf)
            else:
                newRadicand = self.radicand ** other.root * other.radicand ** self.root
                newRoot = self.root * other.root
                return self.arithmeticReturn(newRadicand, newRoot, overwriteSelf)

        elif isinstance(other, Termlike):
            if overwriteSelf:
                self.convertTo(Term(self, other))
                return self
            else:
                return Term(self, other)

        else:
            raise ValueError("unexpected value type. Can only perform arithmetic operations on elements of Termlike type")

    def divide(self, other, overwriteSelf=False):
        if isinstance(other, Rational):
            return self.arithmeticReturn(self.radicand / other ** self.root, self.root, overwriteSelf)

        elif isinstance(other, Root):
            if self.root == other.root:
                return self.arithmeticReturn(self.radicand / other.radicand, self.root, overwriteSelf)
            else:
                newRadicand = self.radicand ** other.root / other.radicand ** self.root
                newRoot = self.root * other.root
                return self.arithmeticReturn(newRadicand, newRoot, overwriteSelf)

        elif isinstance(other, Termlike):
            if overwriteSelf:
                self.convertTo(Fraction(self, other))
                return self
            else:
                return Fraction(self, other)

        else:
            raise ValueError("unexpected value type. Can only perform arithmetic operations on elements of Termlike type")

    def intDivide(self, other):
        # this method is only called if self or other is not numeric
        # TODO: implement

        return 0

    def mod(self, other):
        # this method is only called if self or other is not numeric
        # TODO: implement
        return self - other * (self // other)

    def power(self, other):
        commonDivisor = gcd(other, self.root)
        power = other / commonDivisor
        return Root(self.radicand ** power, self.root / commonDivisor)

    def toRoot(self, other):
        return Root(self.radicand, self.root * other)

    def simplify(self):
        self.radicand.simplify()
        if self.radicand == 0:
            self.convertTo(Rational(0))
        elif self.radicand == 1:
            self.convertTo(Rational(1))
        elif isinstance(self.radicand, Root):
            # remove the nested Root by raising the Term to the appropriate power and raising the parent root accordingly
            nestedRadicand = self.radicand
            self.radicand = nestedRadicand.radicand
            self.root *= nestedRadicand.root
            self.simplify()
        elif self.root == 1:
            self.__class__ = self.radicand.__class__
            self.__dict__ = self.radicand.__dict__
        elif isinstance(self.radicand, Termlike):
            if isinstance(self.radicand, Rational):
                # attempt to move out of the radical any factors that can be reduced by the root
                factor = self.largestRationalFactor(self.radicand)
                if factor != 1:
                    if factor ** self.root == self.radicand:
                        self.__class__ = factor.__class__
                        self.__dict__ = factor.__dict__                        
                    else:
                        self.radicand /= factor ** self.root
                        self *= factor
            elif isinstance(self.radicand, Irrational):
                # attempt to move out of the radical any factors that can be reduced by the root
                if self.radicand.exponent > self.root:
                    baseSymbol = Irrational(self.radicand.code)
                    factoredOutExponent = self.radicand.exponent // self.root
                    if self.radicand.exponent % self.root == 0:
                        newSymbol = baseSymbol ** factoredOutExponent
                        self.__class__ = newSymbol.__class__
                        self.__dict__ = newSymbol.__dict__  
                    else:
                        self.radicand /= baseSymbol ** (factoredOutExponent * self.root)
                        self *= baseSymbol ** factoredOutExponent
            elif isinstance(self.radicand, Symbol):
                # attempt to move out of the radical any factors that can be reduced by the root
                if self.radicand.exponent > self.root:
                    baseSymbol = Symbol(self.radicand.symbol)
                    factoredOutExponent = self.radicand.exponent // self.root
                    if self.radicand.exponent % self.root == 0:
                        newSymbol = baseSymbol ** factoredOutExponent
                        self.__class__ = newSymbol.__class__
                        self.__dict__ = newSymbol.__dict__  
                    else:
                        self.radicand /= baseSymbol ** (factoredOutExponent * self.root)
                        self *= baseSymbol ** factoredOutExponent
            elif isinstance(self.radicand, Imaginary):
                # do nothing
                pass
            elif isinstance(self.radicand, Term):
                simplified = False
                while not(simplified):
                    simplified = True # if any changes are made, reset this to False
                    newRoot = self.root # record any changes to the root, but don't apply them until after processing all subterms
                    terms = copy.deepcopy(self.radicand.getTerms())
                    for t in terms:
                        if isinstance(t, Termlike):
                            if isinstance(t, Expression):
                                # stop searching the term. Cannot simplify roots that contain addition
                                break
                            elif isinstance(t, Root):
                                # remove the nested Root by raising the Term to the appropriate power and raising the parent root correspondingly
                                self.radicand ** t.root
                                newRoot *= t.root
                                simplified = False
                            elif isinstance(t, Rational):
                                # attempt to move out of the radical any factors that can be reduced by the root
                                factor = self.largestRationalFactor(t)
                                if factor != 1:
                                    self.radicand /= factor ** self.root
                                    self *= factor
                                    simplified = False
                            elif isinstance(t, Irrational):
                                # attempt to move out of the radical any factors that can be reduced by the root
                                if t.exponent > self.root:
                                    factoredOutExponent = t.exponent // self.root
                                    baseSymbol = Irrational(t.code)
                                    self.radicand /= baseSymbol ** (factoredOutExponent * self.root)
                                    self *= baseSymbol ** factoredOutExponent
                                    simplified = False
                            elif isinstance(t, Symbol):
                                # attempt to move out of the radical any factors that can be reduced by the root
                                if t.exponent > self.root:
                                    factoredOutExponent = t.exponent // self.root
                                    baseSymbol = Irrational(t.symbol)
                                    self.radicand /= baseSymbol ** (factoredOutExponent * self.root)
                                    self *= baseSymbol ** factoredOutExponent
                                    simplified = False
                            elif isinstance(t, Imaginary):
                                # do nothing
                                pass
                            elif isinstance(t, Term):
                                t.simplify()
                                simplified = False
                        else:
                            raise ValueError("invalid data structure. All terms and subterms should be of Termlike type")

                    # apply the root change
                    self.root = newRoot
            
            if self.extractConstant:
                self.factorConstant(self.primaryIndeterminate)

            return self.copy()
        else:
            raise ValueError("invalid data structure. Radicand should be of Termlike type")

    def evaluateToTerm(self, values=None, variables=None, inplace=False):
        if inplace:
            self.radicand.evaluate(variables=variables, values=values, inplace=inplace)
            self.simplify()
            return self
        else:
            return Root(self.radicand.evaluate(variables=variables, values=values, inplace=inplace), self.root)

    def evaluateToNumber(self, values=None, variables=None, decimalType=False):
        return self.radicand.evaluateToNumber(variables=variables, values=values, decimalType=decimalType) ** (1 / self.root)

    def evaluateNumericalCoefficient(self):
        return self.radicand.evaluateNumericalCoefficient() ** (1 / self.root)

    def isConstant(self, withRespectTo):
        return self.radicand.isConstant(withRespectTo)

    def largestRationalFactor(self, rationalNumber):
        numerator = self.largestIntegerBaseFactor(rationalNumber.numerator)
        denominator = self.largestIntegerBaseFactor(rationalNumber.denominator)

        return Rational(numerator, denominator)

    def largestIntegerBaseFactor(self, number):
        if not(isinstance(number, int)):
            raise ValueError("input must be an integer. Try inputting the numerator or denominator of a rational number")
        
        if self.root == 1:
            return number

        n = 1
        largestN = 1
        while n ** self.root <= number:
            if number % (n ** self.root) == 0:
                largestN = n
            n += 1
        
        return largestN

    def checkIsConstant(self, variableList):
        return self.radicand.isConstant(variableList)

    def getGreatestCommonConstant(self, variableList):
        rootGCD = self.radicand.getGreatestCommonConstant(variableList)
        if rootGCD != 1:
            return Root(rootGCD, self.root)
        else:
            return Rational(1)

    def getNonConstantFactor(self, variableList):
        return Root(self.radicand.getNonConstantFactor(variableList), self.root)

class Irrational(Termlike):
    CODE_PI = "pi"
    CODE_E = "e"
    valueOf = {
        CODE_PI: 3.141592654,
        CODE_E: 2.718284590
    }
    displayString = {
        CODE_PI: "\u03C0",
        CODE_E: "e"
    }
    code = ""
    exponent = 1

    def __init__(self, code, exponent=1):
        if not(isinstance(code, str)):
            raise ValueError("invalid type: code must be of type String")
        else:
            code = code.lower()
            if code in self.valueOf.keys():
                self.code = code
            else:
                raise ValueError("code provided is not supported")

        if not(isinstance(exponent, int)):
            exponent = toRational(exponent)
            if not(isinstance(exponent, Rational)):
                raise ValueError("invalid type: exponent must be a number type: int, Rational, fractions.Fraction, or float")

        self.exponent = exponent

        self.simplify()

    def toString(self):
        self.simplify()
        returnString = self.displayString[self.code]
        if self.exponent != 1:
            returnString += "^" + str(self.exponent)
        return returnString

    def equals(self, other):
        other = copy.deepcopy(other)

        self.simplify()
        if isinstance(self, Irrational):
            if isinstance(other, Termlike):
                other.simplify()
                if isinstance(other, Irrational):
                    return self.code == other.code and self.exponent == other.exponent

            return False
        else:
            return self == other

    def arithmeticReturn(self, exponent, overwiteSelf):
        if overwiteSelf:
            self.exponent = exponent
            self.simplify()
            return self
        else:
            return Irrational(self.code, exponent)

    def add(self, other, overwriteSelf=False):
        if other == self:
            if overwriteSelf:
                self.convertTo(Term(copy.deepcopy(self), Rational(2)))
                return self
            else:
                return Term(copy.deepcopy(self), Rational(2))
        else:
            if overwriteSelf:
                self.convertTo(Expression(copy.deepcopy(self), other))
                return self
            else:
                return Expression(copy.deepcopy(self), other)

    def multiply(self, other, overwriteSelf=False):
        if other.code == self.code:
            return self.arithmeticReturn(self.exponent + other.exponent, overwriteSelf)
        else:
            if overwriteSelf:
                self.convertTo(Term(copy.deepcopy(self), other))
                return self
            else:
                return Term(copy.deepcopy(self), other)

    def divide(self, other, overwriteSelf=False):
        if other.code == self.code:
            return self.arithmeticReturn(self.exponent - other.exponent, overwriteSelf)
        else:
            if overwriteSelf:
                self.convertTo(Fraction(copy.deepcopy(self), other))
                return self
            else:
                return Fraction(copy.deepcopy(self), other)

    def intDivide(self, other):
        # this method is only called if other is not numeric     
        return 0

    def mod(self, other):
        # This method is only called if other is not numeric
        return self.copy()

    def power(self, other):
        return Irrational(self.code, self.exponent * other)

    def toRoot(self, other):
        if self.exponent % other == 0:
            return Irrational(self.code, self.exponent / other)
        else:
            return Root(copy.deepcopy(self), other)

    def simplify(self):
        if self.exponent == 0:
            self.convertTo(Rational(1))
        elif self.exponent < 1:
            self.convertTo(Fraction(1,Irrational(self.code, abs(self.exponent))))
        elif not(isinstance(self.exponent, int)):
            self.exponent = toRational(self.exponent)
            if not(isinstance(self.exponent, Rational)):
                raise ValueError("invalid type: exponent must be a number type: int, Rational, fractions.Fraction, or float")
            else:
                self.convertTo(Root(Irrational(self.code, self.exponent.numerator), self.exponent.denominator))

        return self    
    
    def evaluateToTerm(self, values=None, variables=None, inplace=False):
        return copy.deepcopy(self)

    def evaluateToNumber(self, values=None, variables=None, decimalType=False):
        if decimalType:
            baseValue = decimal(self.valueOf(self.code))
        else:
            baseValue = self.valueOf(self.code)

        return baseValue ** self.exponent
        
    def evaluateNumericalCoefficient(self):
        return self.evaluateToNumber()

class Symbol(Termlike):
    symbol = ""
    exponent = 1

    def __init__(self, symbol, exponent=1):
        if not(isinstance(symbol, str)):
            raise ValueError("invalid type: code must be of type String")
        if len(symbol) > 0:
            if len(symbol) > 1:
                raise ValueError("symbol must be a single character")
            self.symbol = symbol
        else:
            raise ValueError("symbol input cannot be blank")

        if not(isinstance(exponent, int)):
            exponent = toRational(exponent)
            if not(isinstance(exponent, Rational)):
                raise ValueError("invalid type: exponent must be a number type: int, Rational, fractions.Fraction, or float")

        self.exponent = exponent

        self.simplify()

    def toString(self):
        self.simplify()
        returnString = self.symbol
        if self.exponent != 1:
            returnString += "^" + str(self.exponent)
        return returnString

    def equals(self, other):
        return self.symbol == other.symbol and self.exponent == other.exponent

    def arithmeticReturn(self, exponent, overwiteSelf):
        if overwiteSelf:
            self.exponent = exponent
            self.simplify()
            return self
        else:
            return Symbol(self.symbol, exponent)

    def add(self, other, overwriteSelf=False):
        if other == self:
            if overwriteSelf:
                self.convertTo(Term(copy.deepcopy(self), Rational(2)))
                return self
            else:
                return Term(copy.deepcopy(self), Rational(2))
        else:
            if overwriteSelf:
                self.convertTo(Expression(copy.deepcopy(self), other))
                return self
            else:
                return Expression(copy.deepcopy(self), other)

    def multiply(self, other, overwriteSelf=False):
        if other.symbol == self.symbol:
            return self.arithmeticReturn(self.exponent + other.exponent, overwriteSelf)
        else:
            if overwriteSelf:
                self.convertTo(Term(copy.deepcopy(self), other))
                return self
            else:
                return Term(copy.deepcopy(self), other)

    def divide(self, other, overwriteSelf=False):
        if other.symbol == self.symbol:
            return self.arithmeticReturn(self.exponent - other.exponent, overwriteSelf)
        else:
            if overwriteSelf:
                self.convertTo(Fraction(copy.deepcopy(self), other))
                return self
            else:
                return Fraction(copy.deepcopy(self), other)

    def intDivide(self, other):
        if isinstance(other, Symbol) and self.symbol == other.symbol and self.exponent >= other.exponent:
            return 1
        else:
            return 0

    def mod(self, other):
        if self // other > 0:
            return copy.deepcopy(self) - other * (self // other)
        else:
            return copy.deepcopy(self)

    def power(self, other):
        return Symbol(self.symbol, self.exponent * other)

    def toRoot(self, other):
        if self.exponent % other == 0:
            return Symbol(self.symbol, self.exponent / other)
        else:
            return Root(copy.deepcopy(self), other)

    def simplify(self):
        if self.exponent == 0:
            self.convertTo(Rational(1))
        elif self.exponent < 1:
            self.convertTo(Fraction(1,Symbol(self.symbol, abs(self.exponent))))
        elif not(isinstance(self.exponent, int)):
            self.exponent = toRational(self.exponent)
            if not(isinstance(self.exponent, Rational)):
                raise ValueError("invalid type: exponent must be a number type: int, Rational, fractions.Fraction, or float")
            else:
                self.convertTo(Root(Symbol(self.symbol, self.exponent.numerator), self.exponent.denominator))

        return self

    def evaluateToTerm(self, values=None, variables=None, inplace=False):
        evaluationResult = None
        evaluationValue = None
        if variables == None:
            evaluationValue = values
        else:
            if self.symbol in variables:
                evaluationValue = values[variables.index(self.symbol)]
        
        if evaluationValue == None:
            return self
        else:
            evaluationResult = evaluationValue ** self.exponent
            if inplace:
                self.convertTo(evaluationResult)
                return self
            else:
                return evaluationResult

    def evaluateToNumber(self, values=None, variables=None, decimalType=False):
        evaluationResult = None
        evaluationValue = None
        if variables == None:
            evaluationValue = values
        else:
            if self.symbol in variables:
                evaluationValue = values[variables.index(self.symbol)]
        
        if evaluationValue == None:
            raise Exception("cannot evaluate - no value provided for relevant variable")
        else:
            evaluationResult = evaluationValue ** self.exponent
            return evaluationResult.evaluateToNumber(decimalType=decimalType)

    def evaluateNumericalCoefficient(self):
        return 1

    def checkIsConstant(self, variableList):
        return not(self.symbol in variableList)

    def getNonConstantFactor(self, variableList):
        if not(self.checkIsConstant(variableList)):
            return self.copy()
        else:
            return None

class Imaginary(Termlike):
    def __init__(self):
        pass

    def toString(self):
        return("i")

    def equals(self, other):
        return isinstance(other, Imaginary)

    def add(self, other, overwriteSelf=False):
        if other == self:
            if overwriteSelf:
                self.convertTo(Term(copy.deepcopy(self), Rational(2)))
                return self
            else:
                return Term(copy.deepcopy(self), Rational(2))
        else:
            if overwriteSelf:
                self.convertTo(Expression(copy.deepcopy(self), other))
                return self
            else:
                return Expression(copy.deepcopy(self), other)

    def multiply(self, other, overwriteSelf=False):
        if other == self:
            if overwriteSelf:
                self.convertTo(Rational(-1))
                return self
            else:
                return Rational(-1)
        else:
            if overwriteSelf:
                self.convertTo(Term(copy.deepcopy(self), other))
                return self
            else:
                return Term(copy.deepcopy(self), other)

    def divide(self, other, overwriteSelf=False):
        if other == self:
            if overwriteSelf:
                self.convertTo(Rational(1))
                return self
            else:
                return Rational(1)
        else:
            if overwriteSelf:
                self.convertTo(Fraction(copy.deepcopy(self), other))
                return self
            else:
                return Fraction(copy.deepcopy(self), other)

    def intDivide(self, other):
        if other == self:
            return 1
        else:
            return 0

    def mod(self, other):
        if other == self:
            return 0
        else:
            return copy.deepcopy(self)

    def power(self, other):
        if other == 1:
            return copy.deepcopy(self)
        if other % 2 == 0:
            if other % 4 == 0:
                return Rational(1)
            else:
                return Rational(-1)
        else:
            if (other - 1) % 4 == 0:
                return copy.deepcopy(self)
            else:
                return Term(Rational(-1), copy.deepcopy(self))

    def toRoot(self, other):
        return Root(copy.deepcopy(self), other)

    def simplify(self):
        return self
 
    def evaluateToTerm(self, values=None, variables=None, inplace=False):
        return copy.deepcopy(self)

    def evaluateToNumber(self, values=None, variables=None, decimalType=False):
        raise Exception("imaginary number cannot evaluate to real number")

    def evaluateNumericalCoefficient(self):
        return 1

# combinatory classes
class Fraction(Termlike):
    numerator = None
    denominator = None

    def __init__(self, numerator, denominator):
        numerator = copy.deepcopy(numerator)
        numerator = toRational(numerator)
        denominator = copy.deepcopy(denominator)
        denominator = toRational(denominator)

        if not(isinstance(numerator, Termlike)):
            raise ValueError("invalid input type: numerator must be of Termlike type or number type (int, float, fractions.Fraction)")
        if not(isinstance(denominator, Termlike)):
            raise ValueError("invalid input type: denominator must be of Termlike type or number type (int, float, fractions.Fraction)")

        self.numerator = numerator
        self.denominator = denominator

        self.simplify()

    def toString(self):
        return "[(" + str(self.numerator) + ")/(" + str(self.denominator) + ")]"

    def equals(self, other):
        return self.numerator == other.numerator and self.denominator == other.denominator

    def arithmeticReturn(self, numerator, denominator, overwiteSelf):
        if overwiteSelf:
            self.numerator = numerator
            self.denominator = denominator
            self.simplify()
            return self
        else:
            return Fraction(numerator, denominator)

    def add(self, other, overwriteSelf=False):
        if self.denominator == other.denominator:
            return self.arithmeticReturn(self.numerator + other.numerator, self.denominator, overwriteSelf)
        else:
            # use cross multiplication
            return self.arithmeticReturn(self.numerator * other.denominator + other.numerator * self.denominator, self.denominator * other.denominator, overwriteSelf)

    def multiply(self, other, overwriteSelf=False):
        return self.arithmeticReturn(self.numerator * other.numerator, self.denominator * other.denominator, overwriteSelf)

    def divide(self, other, overwriteSelf=False):
        return self.arithmeticReturn(self.numerator * other.denominator, self.denominator * other.numerator, overwriteSelf)

    def intDivide(self, other):
        # TODO: implement
        pass

    def mod(self, other):
        # TODO: implement
        pass

    def power(self, other):
        return Fraction(self.numerator ** other, self.denominator ** other)

    def toRoot(self, other):
        return Root(copy.deepcopy(self), other)

    def simplify(self):
        self.numerator.simplify()
        self.denominator.simplify()

        if self.numerator == self.denominator:
            self.convertTo(Rational(1))
        elif isinstance(self.denominator, Rational)  :
            self.convertTo(Term(self.numerator, 1 / self.denominator))
        elif isinstance(self.denominator, Fraction):
            self.numerator *= self.denominator.denominator
            self.denominator = self.denominator.numerator
            if isinstance(self.numerator, Fraction):
                self.denominator *= self.numerator.denominator
                self.numerator = self.numerator.numerator
        elif isinstance(self.numerator, Fraction):
            self.denominator *= self.numerator.denominator
            self.numerator = self.numerator.numerator
        else:
            commonFactor = gcd(self.numerator, self.denominator)
            self.numerator /= commonFactor
            self.denominator /= commonFactor

        return self
 
    def evaluateToTerm(self, values=None, variables=None, inplace=False):
        if inplace:
            self.numerator.evaluateToTerm(values=values, variables=variables, inplace=inplace)
            self.denominator.evaluateToTerm(values=values, variables=variables, inplace=inplace)
            return self.simplify()
        else:
            newNumerator = copy.deepcopy(self.numerator.evaluateToTerm(values=values, variables=variables, inplace=inplace))
            newDenominator = copy.deepcopy(self.denominator.evaluateToTerm(values=values, variables=variables, inplace=inplace))
            return Fraction(newNumerator, newDenominator)

    def evaluateToNumber(self, values=None, variables=None, decimalType=False):
        return self.numerator.evaluateToNumber(values=values, variables=variables, decimalType=decimalType) / self.denominator.evaluateToNumber(values=values, variables=variables, decimalType=decimalType)

    def evaluateNumericalCoefficient(self):
        return self.numerator.evaluateNumericalCoefficient() / self.denominator.evaluateNumericalCoefficient()

    def checkIsConstant(self, variableList):
        self.simplify()
        return self.numerator.isConstant(variableList) and self.denominator.isConstant(variableList)

class Term(Termlike):
    __terms = []

    def __init__(self, *terms):
        if len(terms) == 0:
            terms = (Rational(1))

        terms = copy.deepcopy(terms)
        self.__terms = list(terms)
        self.simplify()

    def toString(self):
        self.simplify()

        rationalString = ""
        irrationalString = ""
        rootString = ""
        imaginaryString = ""
        expressionString = ""
        fractionString = ""
        symbolString = ""
        symbolDict = {}

        for term in self.__terms:
            if isinstance(term, Rational):
                rationalString += str(term)
            elif isinstance(term, Irrational):
                irrationalString += str(term)
            elif isinstance(term, Root):
                rootString += "[" + str(term) + "]"
            elif isinstance(term, Imaginary):
                imaginaryString += str(term)
            elif isinstance(term, Expression):
                expressionString += "(" + str(term) + ")"
            elif isinstance(term, Fraction):
                expressionString += "[" + str(term) + "]"
            elif isinstance(term, Symbol):
                symbolDict[term.symbol] = term
            else:
                raise ValueError("invalid data structure. Unexpected type {type} encountered in Term".format(type = term.__class__))

        # sort the symbols by variable
        for key in symbolDict.keys():
            symbolString += str(symbolDict[key])

        return rationalString + irrationalString + rootString + imaginaryString + expressionString + fractionString + symbolString

    def equals(self, other):
        # make a copy of both inputs and simplify according to a common organizational standard
        A = self.copy()
        B = other.copy()
        A.setExtractConstant(False)
        B.setExtractConstant(False)
        A.setOrganization(Termlike.ORGANIZATION_TYPE_SEPARATE_TERMS)
        B.setOrganization(Termlike.ORGANIZATION_TYPE_SEPARATE_TERMS)
        
        # check whether the new organization turned either object into an Expression
        if isinstance(A, Expression):
            if isinstance(B, Expression):
                return A == B
            else:
                return False
        else:
            if isinstance(B, Expression):
                return False
            else:
                # both objects are still Terms
                termTypes = [
                    Rational,
                    Root,
                    Irrational,
                    Symbol,
                    Imaginary,
                    Fraction,
                    Term,
                    Expression
                ]
                for termType in termTypes:
                    A_collection = []
                    B_collection = []
                    for term in A.__terms:
                        if isinstance(term, termType):
                            A_collection.append(term)
                    for term in B.__terms:
                        if isinstance(term, termType):
                            B_collection.append(term)

                    if len(A_collection) != len(B_collection):
                        return False

                    while len(A_collection > 0):
                        matchFound = False
                        for i in range(len(B_collection)):
                            if A_collection[0] == B_collection[i]:
                                matchFound = True
                                del A_collection[0]
                                del B_collection[i]
                                break
                        if not(matchFound):
                            return False
                
                return True

    def add(self, other, overwriteSelf=False):
        if self == other:
            if overwriteSelf:
                self *= 2
                return self
            else:
                return 2 * self
        else:
            if overwriteSelf:
                self.convertTo(Expression(self, other))
                return self
            else:
                return Expression(self, other)

    def multiply(self, other, overwriteSelf=False):
        if overwriteSelf:
            self.__terms.extend(other.__terms)
            self.simplify()
            return self
        else:
            return Term(self, other)

    def divide(self, other, overwriteSelf=False):
        if overwriteSelf:
            self.convertTo(Fraction(self, other))
            return self
        else:
            return Fraction(self, other)

    def intDivide(self, other):
        if isinstance(other, Expression):
            return 0
        else:
            selfTermsByType = self.getTermsByType()
            otherTermsByType = other.getTermsByType()
            # numeric portion
            numericDivision = 1
            if (Termlike.TYPE_ROOT in selfTermsByType) or (Termlike.TYPE_ROOT in otherTermsByType):
                selfNumeric = selfTermsByType[Termlike.TYPE_RATIONAL]
                otherNumeric = otherTermsByType[Termlike.TYPE_RATIONAL]
                if Termlike.TYPE_ROOT in selfTermsByType:
                    selfNumeric += selfTermsByType[Termlike.TYPE_ROOT]
                if Termlike.TYPE_ROOT in otherTermsByType:
                    otherNumeric += otherTermsByType[Termlike.TYPE_ROOT]
                numericDivision = selfNumeric // otherNumeric

            # irrational portion
            irrationalDivision = None
            if Termlike.TYPE_IRRATIONAL in otherTermsByType:
                irrationalDivision = 1
                if Termlike.TYPE_IRRATIONAL in selfTermsByType:
                    for irrationalFactor in otherTermsByType[Termlike.TYPE_IRRATIONAL]:
                        pass

    def mod(self, other):
        pass

    def power(self, other):
        pass

    def toRoot(self, other):
        pass

    def simplify(self):
        simplified = False
        while not(simplified):
            simplified = True # set to False if anything needs to be rechecked
            # clean up data structure
            for t in copy.deepcopy(self.__terms):
                if isinstance(t, Termlike):
                    if isinstance(t, Term):
                        self.simplifyNestedTerm(t)
                        self.__terms.remove(t)
                elif isinstance(t, (fractions.Fraction, int, float, decimal)):
                    self.__terms.remove(t)
                    self.__terms.append(toRational(t))
                else:
                    raise ValueError("invalid input. All terms should be of Termlike type")

            # mergeRedundancies        
            rationals = []
            irrationals = []
            roots = []
            imaginaries = []
            fractions = []
            symbols = []

            for t in self.__terms:
                if isinstance(t, Rational):
                    rationals.append(t)
                if isinstance(t, Irrational):
                    irrationals.append(t)
                elif isinstance(t, Root):
                    roots.append(t)
                elif isinstance(t, Imaginary):
                    imaginaries.append(t)
                elif isinstance(t, Fraction):
                    fractions.append(t)
                elif isinstance(t, Symbol):
                    symbols.append(t)

            # merge all Rationals into a single object
            for i in range(1, len(rationals)):
                rationals[0] *= rationals[i]
                self.__terms.remove(rationals[i])

            # merge all Roots into a single object
            for i in range(1, len(roots)):
                roots[0] *= roots[i]
                self.__terms.remove(roots[i])
                simplified = False

            # merge all imaginaries into a single object, or cancel them out
            if len(imaginaries) > 1:
                # cancel out imaginary numbers. If count is odd, one will be left. Otherwise all will be removed
                for n in range(len(imaginaries) % 2, len(imaginaries)):
                    self.__terms.remove(imaginaries[n])
                cancellations = len(imaginaries) // 2
                negativeResult = cancellations % 2 == 1
                if negativeResult:
                    self.__terms.append(Rational(-1))
                    simplified = False

            # merge all Fractions into a single object
            for i in range(1, len(fractions)):
                fractions[0] *= fractions[i]
                self.__terms.remove(fractions[i])

            # merge any irrationals that share the same code
            if len(irrationals) > 1:
                codeSet = set()
                for r in irrationals:
                    codeSet.add(r.code)
                for code in codeSet:
                    totalExponent = 0
                    for r in (r for r in irrationals if r.code == code):
                        totalExponent += r.exponent
                        self.__terms.remove(r)
                    self.__terms.append(Irrational(code, totalExponent))
            
            # merge any symbols that share the same symbol
            if len(symbols) > 1:
                symbolSet = set()
                for s in symbols:
                    symbolSet.add(s.code)
                for symbol in symbolSet:
                    totalExponent = 0
                    for s in (s for s in symbols if s.symbol == symbol):
                        totalExponent += s.exponent
                        self.__terms.remove(s)
                    self.__terms.add(Symbol(symbol, totalExponent))
        
        return self

    def simplifyNestedTerm(self, nestedTerm):
        for t in copy.deepcopy(nestedTerm.__terms):
            if isinstance(t, Termlike):
                if isinstance(t, Term):
                    self.simplifyNestedTerm(t)
                else:
                    self.__terms.append(t)
            else:
                raise ValueError("invalid input. All terms should be of Termlike type")

    def evaluateToTerm(self, values=None, variables=None, inplace=False):
        pass

    def evaluateToNumber(self, values=None, variables=None, decimalType=False):
        pass

    def evaluateNumericalCoefficient(self):
        pass

    def checkIsConstant(self, variableList):
        for term in self.__terms:
            if not(term.checkIsConstant(variableList)):
                return False
        return True

    def getGreatestCommonConstant(self, variableList):
        constant = Rational(1)
        for term in self.__terms:
            constant *= term.getGreatestCommonConstant(variableList)

        return constant

    def getNonConstantFactor(self, variableList):
        variableTerm = None
        for term in self.__terms:
            variablePortion = term.getNonConstantFactor(variableList)
            if variablePortion != None:
                if variableTerm == None:
                    variableTerm = variablePortion
                else:
                    variableTerm *= variablePortion
        
        return variableTerm

    def separateCommonConstant(self):
        constantTerm = None
        variableTerm = None
        for term in self.__terms:
            if term.isConstant(self.primaryIndeterminate):
                if constantTerm == None:
                    constantTerm = term.copy()
                else:
                    constantTerm *= term
            else:
                if isinstance(term, Expression):
                    term.separateCommonConstant()
                    greatestCommonConstantFactor = term.getGreatestCommonConstant(self.primaryIndeterminate)

    def getTerms(self):
        return copy.deepcopy(self.__terms)

    def reorganize(self):
        if self.organization == self.ORGANIZATION_TYPE_DO_NOT_SIMPLIFY:
            if self.extractConstant:
                constantTerm = None
                variableTerm = None
                for term in self.__terms:
                    if term.isConstant(self.primaryIndeterminate):
                        if constantTerm == None:
                            constantTerm = term.copy()
                        else:
                            constantTerm *= term
                    else:
                        if isinstance(term, Expression):
                            greatestCommonConstantFactor = term.getGreatestCommonConstant(self.primaryIndeterminate)
            else:
                return
        else:
            nonExpressionTerm = None
            expandedExpression = None
            for term in self.__terms:
                if isinstance(term, Expression):
                    if expandedExpression == None:
                        expandedExpression = copy.deepcopy(term)
                    else:
                        expandedExpression *= term
                else:
                    if nonExpressionTerm == None:
                        nonExpressionTerm = copy.deepcopy(term)
                    else:
                        nonExpressionTerm *= term

            if expandedExpression == None:
                pass
            else:
                if nonExpressionTerm == None:
                    self.convertTo(expandedExpression)
                else:
                    self.convertTo(expandedExpression.multiplyThrough(nonExpressionTerm))

    def getTermsByType(self):
        self.simplify()
        rationalComponent = Rational(1)
        rootComponent = None
        irrationalComponents = []
        imaginaryComponent = None
        fractionComponents = []
        symbolComponents = []
        expressionComponents = []

        for t in self.__terms:
            if isinstance(t, Rational):
                rationalComponent *= t
            elif isinstance(t, Root):
                if rootComponent == None:
                    rootComponent = t.copy()
                else:
                    rootComponent *= t
            elif isinstance(t, Irrational):
                irrationalComponents.append(t)
            elif isinstance(t, Imaginary):
                if imaginaryComponent == None:
                    imaginaryComponent = t.copy()
                else:
                    imaginaryComponent = None
                    rationalComponent *= -1
            elif isinstance(t, Fraction):
                fractionComponents.append(t)
            elif isinstance(t, Symbol):
                symbolComponents.append(t)
            elif isinstance(t, Expression):
                expressionComponents.append(t)
            else:
                raise Exception("unsupported data type in Term.__terms")

        sortedTerms = {}
        sortedTerms[Termlike.TYPE_RATIONAL] = rationalComponent
        if rootComponent != None:
            sortedTerms[Termlike.TYPE_ROOT] = rootComponent
        if len(irrationalComponents) > 0:
            sortedTerms[Termlike.TYPE_IRRATIONAL] = irrationalComponents
        if imaginaryComponent != None:
            sortedTerms[Termlike.TYPE_IMAGINARY] = imaginaryComponent
        if len(fractionComponents) > 0:
            sortedTerms[Termlike.TYPE_FRACTION] = fractionComponents
        if len(symbolComponents) > 0:
            sortedTerms[Termlike.TYPE_SYMBOL] = symbolComponents
        if len(expressionComponents) > 0:
            sortedTerms[Termlike.TYPE_EXPRESSION] = expressionComponents
        
        return sortedTerms

class Expression(Term):
    __terms = []

    def __init__(self, *terms):
        if len(terms) == 0:
            terms = (Rational(1))

        terms = copy.deepcopy(terms)
        self.__terms = list(terms)
        self.simplify()

    def getTermsByType(self):
        self.simplify()
        rationalComponent = Rational(1)
        rootComponent = None
        irrationalComponents = []
        imaginaryComponent = None
        fractionComponents = []
        symbolComponents = []
        expressionComponents = []

        for t in self.__terms:
            if isinstance(t, Rational):
                rationalComponent *= t
            elif isinstance(t, Root):
                if rootComponent == None:
                    rootComponent = t.copy()
                else:
                    rootComponent *= t
            elif isinstance(t, Irrational):
                irrationalComponents.append(t)
            elif isinstance(t, Imaginary):
                if imaginaryComponent == None:
                    imaginaryComponent = t.copy()
                else:
                    imaginaryComponent = None
                    rationalComponent *= -1
            elif isinstance(t, Fraction):
                fractionComponents.append(t)
            elif isinstance(t, Symbol):
                symbolComponents.append(t)
            elif isinstance(t, Expression):
                expressionComponents.append(t)
            else:
                raise Exception("unsupported data type in Term.__terms")

        sortedTerms = {}
        sortedTerms[Termlike.TYPE_RATIONAL] = rationalComponent
        if rootComponent != None:
            sortedTerms[Termlike.TYPE_ROOT] = rootComponent
        if len(irrationalComponents) > 0:
            sortedTerms[Termlike.TYPE_IRRATIONAL] = irrationalComponents
        if imaginaryComponent != None:
            sortedTerms[Termlike.TYPE_IMAGINARY] = imaginaryComponent
        if len(fractionComponents) > 0:
            sortedTerms[Termlike.TYPE_FRACTION] = fractionComponents
        if len(symbolComponents) > 0:
            sortedTerms[Termlike.TYPE_SYMBOL] = symbolComponents
        if len(expressionComponents) > 0:
            sortedTerms[Termlike.TYPE_EXPRESSION] = expressionComponents
        
        return sortedTerms






i = Imaginary()

print(i ** 3)



# B1 = Rational(6)
# B2 = Root(3)
# B = Term([B1, B2])
# print(B.__class__)
# print(B.getTerms())

# A = Root(32, 2)
# print(A.__class__)
# print(A.getTerms())
# print()
# print(A)
# A2 = Root(3, 2)
# B = Rational(6)
# B2 = Rational(9,2)
# C = Term([A, A2, B, B2])

# print()
# print(C)

# A = Root(32,2)
# B = Root(8,2)
# print(A)
# print(B)

# print(A // B)
# print(C)
# print(D)
# print()
# print(C ** 0.5)

# A = 0.02
# print(math.ceil(math.log10(abs(A))))