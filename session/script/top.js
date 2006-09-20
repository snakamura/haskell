function showTip(item, id, type) {
	var tip = null;
	var text = false;
	if (type.match(/^image\//)) {
		tip = '<img src="content.cgi?id=' + id + '" onload="return adjustImageSize(this)"/>';
	}
	else if (type.match(/^text\//)) {
		tip = '<div><pre id="tipContent">Loading...</pre></div>';
		text = true;
	}
	overlib(tip, FULLHTML);
	
	if (text)
		var ajax = new Ajax.Updater('tipContent',
									'content.cgi?id=' + id,
									{
										method: 'get',
										asynchronous: true
									});
}

function hideTip(item) {
	return nd();
}

function adjustImageSize(img) {
	if (img.width > 200)
		img.width = 200;
	img.visibility = true;
}
