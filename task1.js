var FIG_SQUARE = 'square';
var FIG_RECTANGLE = 'rectangle';
var FIG_RHOMBUS = 'rhombus';
var FIG_PARALLELOGRAM = 'parallelogram';
var FIG_QUADRANGLE = 'quadrangle';
var FIG_OTHER = 'other';

// Formula taken from http://acmp.ru/article.asp?id_text=170
function _intersects(ax1, ay1, ax2, ay2, bx1, by1, bx2, by2) {
    var v1 = (bx2 - bx1) * (ay1 - by1) - (by2 - by1) * (ax1 - bx1);
    var v2 = (bx2 - bx1) * (ay2 - by1) - (by2 - by1) * (ax2 - bx1);
    var v3 = (ax2 - ax1) * (by1 - ay1) - (ay2 - ay1) * (bx1 - ax1);
    var v4 = (ax2 - ax1) * (by2 - ay1) - (ay2 - ay1) * (bx2 - ax1);
    return (v1 * v2 < 0) && (v3 * v4 < 0);
}

function intersects(p1, p2, p3, p4) {
    var params = [].concat(p1, p2, p3, p4);
    return _intersects.apply(null, params);
}

// Make a figure independant of the points order.
// At least two segments must cross, otherwise return points as is.
function sortPoints(p1, p2, p3, p4) {
    if (intersects(p1, p2, p3, p4)) {
        return [p1, p3, p2, p4];
    }
    if (intersects(p1, p3, p2, p4)) {
        return [p1, p2, p3, p4];
    }
    if (intersects(p1, p4, p2, p3)) {
        return [p1, p2, p4, p3];
    }
    return [p1, p2, p3, p4];
}

function distance(p1, p2) {
    return Math.sqrt(
        Math.pow(p2[0] - p1[0], 2) +
        Math.pow(p2[1] - p1[1], 2)
    );
}

function isQuadrangle(p1, p2, p3, p4) {
    return intersects(p1, p3, p2, p4);
}

function isOther(p1, p2, p3, p4) {
    return !isQuadrangle(p1, p2, p3, p4);
}

function isRectangle(p1, p2, p3, p4) {
    var diag1 = distance(p1, p3);
    var diag2 = distance(p2, p4);
    return isQuadrangle(p1, p2, p3, p4) && diag1 === diag2;
}

function isSquare(p1, p2, p3, p4) {
    var side1 = distance(p1, p2);
    var side2 = distance(p1, p4);
    return isRectangle(p1, p2, p3, p4) && side1 === side2;
}

function isParallelogram(p1, p2, p3, p4) {
    var diag1 = distance(p1, p3);
    var diag2 = distance(p2, p4);
    var side1 = distance(p1, p2);
    var side2 = distance(p2, p3);
    var side3 = distance(p3, p4);
    var side4 = distance(p4, p1);
    return (
        isQuadrangle(p1, p2, p3, p4) &&
        diag1 !== diag2 && side1 === side3 &&
        side2 == side4
    );
}

function isRhombus(p1, p2, p3, p4) {
    var side1 = distance(p1, p2);
    var side2 = distance(p2, p3);
    return (
        isParallelogram(p1, p2, p3, p4) && side1 === side2
    );
}

function getFigure(p1, p2, p3, p4) {
    points = sortPoints(p1, p2, p3, p4);
    var pairs = [
        [isSquare, FIG_SQUARE],
        [isRectangle, FIG_RECTANGLE],
        [isRhombus, FIG_RHOMBUS],
        [isParallelogram, FIG_PARALLELOGRAM],
        [isQuadrangle, FIG_QUADRANGLE],
        [isOther, FIG_OTHER]
    ];
    for (var i = 0; i < pairs.length; i++) {
        var func = pairs[i][0];
        var figure = pairs[i][1];
        if (func.apply(null, points)) {
            return figure;
        }
    }
}

// tests

function test(p1, p2, p3, p4, expected) {
    var result = getFigure(p1, p2, p3, p4);
    if (expected === result) {
        console.log('OK ' + points + ' = ' + expected);
    }
    else {
        console.error('ERROR ' + points + ' != ' + expected + ' (' + result + ')');
    }
}


test([0, 1], [1, 1], [1, 0], [0, 0], FIG_SQUARE);
test([-1, 1], [2, 1], [2, -2], [-1, -2], FIG_SQUARE); // offset
test([-1, 2], [1, 3], [2, 1], [0, 0], FIG_SQUARE); // rotated

test([0, 2], [5, 2], [5, 0], [0, 0], FIG_RECTANGLE);
test([-10, -3], [-5, -3], [-5, -10], [-10, -10], FIG_RECTANGLE); // offset
test([-2, 2], [5, 5], [3, 7],  [0, 0], FIG_RECTANGLE); // rotated
test([2, -2], [-5, -5], [-3, -7],  [0, 0], FIG_RECTANGLE); // rotated

test([-2, 0], [0, 5], [2, 0], [0, -5], FIG_RHOMBUS);
test([-4, 4], [0, 2], [2, -2], [-2, 0], FIG_RHOMBUS); // rotated
test([-10, 0], [0, 1], [10, 0], [0, -1], FIG_RHOMBUS); // quite flat

test([1, 2], [3, 2], [2, 0], [0, 0], FIG_PARALLELOGRAM);
test([2, 3], [4, -3], [2, -6], [0, 0], FIG_PARALLELOGRAM); // rotated

test([0, 0], [-2, 1], [-3, 10], [5, 3], FIG_QUADRANGLE);
test([0, 2], [2, 2], [2, -1], [0, 0], FIG_QUADRANGLE);

test([-1, 0], [0, 3], [1, 0], [0, 1], FIG_OTHER); // 5 angles
test([-1, 2], [2, 0], [-1, -1], [0, 0], FIG_OTHER); // 5 angles

test([0, 1], [1, 0], [1, 1], [0, 0], FIG_SQUARE); // points order
test([-2, 2], [0, 0], [3, 7], [5, 5], FIG_RECTANGLE); // points order
test([-4, 4], [-2, 0], [0, 2], [2, -2], FIG_RHOMBUS); // points order
