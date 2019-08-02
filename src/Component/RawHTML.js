exports.setInnerHTMLImpl = function (unit, html, element) {
    element.innerHTML = html;
    return unit;
}