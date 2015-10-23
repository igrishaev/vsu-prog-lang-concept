#coding=utf-8


class cnum(object):

    def __init__(self, a=0, b=0):
        self.a = a
        self.b = b

    def __repr__(self):
        return '%g%+gi' % (self.a, self.b)

    __str__ = __repr__

    def __eq__(self, other):
        return (
            self.a == other.a
            and self.b == other.b
        )

    def __add__(self, other):
        return cnum(self.a + other.a, self.b + other.b)

    def __sub__(self, other):
        return cnum(self.a - other.a, self.b - other.b)

    def __mul__(self, other):
        return cnum(
            self.a * other.a - self.b * other.b,
            self.a * other.b + self.b * other.a
        )

    def __div__(self, other):
        return cnum(
            (self.a * other.a + self.b * other.b) / float(other.a ** 2  + other.b ** 2),
            (other.a  * self.b - self.a * other.b) / float(other.a ** 2  + other.b ** 2),
        )


# tests


import unittest


class Tests(unittest.TestCase):

    def test_str(self):
        self.assertEqual(str(cnum(1, 2)), '1+2i')
        self.assertEqual(str(cnum(-5, -3)), '-5-3i')

    def test_immutable(self):
        c = cnum(1, 2)
        res = c * c + c - cnum(99, -99)
        self.assertEqual(c, cnum(1, 2))

    def test_eq(self):
        self.assertEqual(cnum(1, 2), cnum(1, 2))

    def test_non_eq(self):
        self.assertNotEqual(cnum(2, 2), cnum(1, 2))
        self.assertNotEqual(cnum(1, 2), cnum(1, 1))

    def test_add(self):
        self.assertEqual(cnum(5, -2) + cnum(-1, 4),
                         cnum(4, 2))

    def test_sub(self):
        self.assertEqual(cnum(5, -2) - cnum(-1, 4),
                         cnum(6, -6))

    def test_mul(self):
        self.assertEqual(cnum(2, 3) * cnum(-1, 1),
                         cnum(-5, -1))

    def test_div(self):
        self.assertEqual(cnum(-2, 1) / cnum(1, -1),
                         cnum(-1.5, -0.5))


if __name__ == '__main__':
    unittest.main()
