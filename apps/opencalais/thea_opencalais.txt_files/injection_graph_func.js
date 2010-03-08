//CALL BUTTON
var SkypeActiveCallButtonPart = 0;
var skype_aFreeCallIds = new Array();
//var skype_sPathPrefix = "skype_ff_toolbar_win/"; //chrome://skype_ff_toolbar_win/content/
var skype_sPathPrefix = "chrome://skype_ff_toolbar_win/content/";

function SkypeSetCallButtonPart(obj)
{
	if (obj.getAttribute('id') == '__skype_highlight_id_left')
	{
		SkypeActiveCallButtonPart = 0;
	}
	else if (obj.getAttribute('id') == '__skype_highlight_id_right')
	{
		SkypeActiveCallButtonPart = 1;
	}
}

function skype_getCallButtonsParts(obj)
{
	var res = new Object();
	res.cb_part_l = null;
	res.cb_part_ml = null;
	res.cb_part_mr = null;
	res.cb_part_r = null;

	if (obj.getAttribute('rtl') == 'false')
	{
		res.cb_part_l = obj.firstChild.firstChild;
		res.cb_part_ml = obj.firstChild.lastChild;
		res.cb_part_mr = obj.lastChild.firstChild;
		res.cb_part_r = obj.lastChild.lastChild;

		res.cb_flag = obj.firstChild.lastChild.firstChild;
		if (res.cb_flag && res.cb_flag.isSameNode(obj.firstChild.firstChild.firstChild) == true)
			res.cb_flag = null;
	}
	else
	{
		res.cb_part_l = obj.lastChild.lastChild;
		res.cb_part_ml = obj.lastChild.firstChild;
		res.cb_part_mr = obj.firstChild.lastChild;
		res.cb_part_r = obj.firstChild.firstChild;

		res.cb_flag = obj.lastChild.firstChild.lastChild;
		if (res.cb_flag && res.cb_flag.isSameNode(obj.lastChild.lastChild.lastChild) == true)
			res.cb_flag = null;
	}

	return res;
}

function skype_makeFreeCall(obj, isInternational, isFax)
{     
    var doc = window._content.document;	
	if (!doc) return;	
	var originalNode = doc.createElementNS('http://www.w3.org/1999/xhtml', "span");		
	originalNode.setAttribute("id", "__skype_highlight_origignal_node");
	originalNode.setAttribute("class", "skype_tb_injection_innerTextOriginal");
	var originalText = doc.createTextNode(obj.getAttribute('context'));
	originalNode.appendChild(originalText);
	obj.parentNode.insertBefore(originalNode, obj);

	var parts = skype_getCallButtonsParts(obj);
	obj.setAttribute("freecall", "true" );
	
	var name = skype_getSettingById("skype_message_msgFreeCallTooltip").getAttribute("value");
	parts.cb_part_mr.parentNode.setAttribute("title", name);	
	parts.cb_part_ml.firstChild.src = skype_sPathPrefix + "icons/skypeicon_16x16.gif";
	parts.cb_part_ml.firstChild.style.backgroundImage = "none";
	parts.cb_part_ml.firstChild.style.height = "12px";

	parts.cb_part_l.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_normal_l_freecall.gif')";
	if (parts.cb_part_l.isSameNode(parts.cb_part_ml) != true)
		parts.cb_part_ml.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_normal_m_freecall.gif')";		
	parts.cb_part_mr.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_normal_m_freecall.gif')";
	parts.cb_part_r.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_normal_r_freecall.gif')";
	obj.lastChild.firstChild.setAttribute('class', 'skype_tb_innerText_freecall');
	SkypeSetCallButton(obj,0,false,false,null);	
}

function skype_isFreeCall(obj)
{
	return (obj.getAttribute('freecall') == null || obj.getAttribute('freecall') == "0") ? false : true;
}

function SkypeSetCallButton(obj, hl, isInternational, isFax, _event)
{
	var callButtonsParts = skype_getCallButtonsParts(obj);
	var cb_part_l  = callButtonsParts.cb_part_l;
	var cb_part_ml = callButtonsParts.cb_part_ml;
	var cb_part_mr = callButtonsParts.cb_part_mr;
	var cb_part_r  = callButtonsParts.cb_part_r;
	var cb_flag    = callButtonsParts.cb_flag;
	var isFreeCall = skype_isFreeCall(obj);

	if (hl == 1)
	{
		if (isFreeCall)
		{	
		    try{	    
		    var innerTextObj = obj.lastChild.firstChild;
			var coords = getElementAbsolutePos(innerTextObj);
			var x2 = coords.x + innerTextObj.offsetWidth;
			var y2 = coords.y + innerTextObj.offsetHeight;
            
		    /*if ((_event.pageX > coords.x) && (_event.pageY > coords.y) && (_event.pageX < x2) && (_event.pageY < y2)) 
			{		
			    return;
			}*/			

			obj.lastChild.firstChild.removeChild(obj.lastChild.firstChild.lastChild);			
			if ( (typeof(innerTextObj.lastChild.id) != "undefined") && (innerTextObj.lastChild.id == "freecallLabel") )
			{
				obj.lastChild.firstChild.removeChild(obj.lastChild.firstChild.lastChild);
			}			
			obj.lastChild.firstChild.innerHTML += obj.getAttribute('context');
			}
			catch(e)
			{alert(e);}
		}
		else
		{
			cb_part_l.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_mouseover_l.gif')";
			if (cb_part_l.isSameNode(cb_part_ml) != true)
				cb_part_ml.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_mouseover_m.gif')";
			cb_part_mr.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_mouseover_m.gif')";

			if (isInternational == "0")
			{
				if (SkypeActiveCallButtonPart == 0)    //left
				{
					cb_part_r.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_mouseonflag_r"+(isFax?"_fax":"")+".gif')";
					//shadow
					if (cb_flag)
					{
						cb_flag.style.top = '1px';
						cb_flag.style.left = '1px';
						/*top right bottom left*/
						cb_flag.style.padding = '1px 0px 0px 1px';//'2px 0px 0px 0px';
					}
				}
				else                            //right
				{
					cb_part_r.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_mouseover_r"+(isFax?"_fax":"")+".gif')";
					//flag
					if (cb_flag)
					{
						cb_flag.style.top = '0px';
						cb_flag.style.left = '0px';
						cb_flag.style.padding = '0px 1px 1px 0px';//'0px 1px 1px 0px';
						cb_flag.style.margin = '0px 0px 2px 0px;';
					}
				}
			}
			else
			{
				cb_part_r.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_mouseover_r"+(isFax?"_fax":"")+".gif')";
				//flag
				if (cb_flag)
				{
					cb_flag.style.top = '0px';
					cb_flag.style.left = '0px';
					cb_flag.style.padding = '0px 1px 1px 0px';
					cb_flag.style.margin = '0px 0px 2px 0px;';
				}
			}
		}
	}
	else
	{
		if (isFreeCall)
		{			
			var innerTextObj = obj.lastChild.firstChild;
			if (typeof(innerTextObj.lastChild.tagName) == "undefined")
			{
				var blockWidth = innerTextObj.offsetWidth - 60; // 56px - width of freecall_label.gif, 4px - padding-left
				var inject = "<img src='" + skype_sPathPrefix + "freecall_label.gif' id='freecallLabel'>";

				if (blockWidth > 0)
				{
					inject += "<img src='" + skype_sPathPrefix + "space.gif' style='height: 1px; width: " + blockWidth + "px;' id='freecallSpace'>";
				}

				obj.lastChild.firstChild.lastChild.nodeValue = "";
				innerTextObj.innerHTML += inject;
			}
		}
		else
		{
			cb_part_l.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_normal_l.gif')";
			if (cb_part_l.isSameNode(cb_part_ml) != true)
				cb_part_ml.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_normal_m.gif')";
			cb_part_mr.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_normal_m.gif')";
			cb_part_r.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_normal_r"+(isFax?"_fax":"")+".gif')";

			//flag
			if (cb_flag)
			{
				cb_flag.style.top = '0px';
				cb_flag.left = '0px';
				cb_flag.style.padding = '0px 1px 1px 0px';
				cb_flag.style.margin = '0px 0px 2px 0px;';
			}
		}
	}
}
var skype_cb_l = '';
var skype_cb_m = '';
var skype_cb_r = '';

function SkypeSetCallButtonPressed(obj, pr, isInternational, isFax)
{
	var callButtonsParts = skype_getCallButtonsParts(obj);
	var cb_part_l  = callButtonsParts.cb_part_l;
	var cb_part_ml = callButtonsParts.cb_part_ml;
	var cb_part_mr = callButtonsParts.cb_part_mr;
	var cb_part_r  = callButtonsParts.cb_part_r;
	var cb_flag    = callButtonsParts.cb_flag;
	var isFreeCall = skype_isFreeCall(obj);

	if (!isFreeCall)
	{
		if (pr == 1)
		{
			skype_cb_l = cb_part_l.style.backgroundImage;//getAttribute('src');
			skype_cb_m = cb_part_mr.style.backgroundImage;
			skype_cb_r = cb_part_r.style.backgroundImage;//getAttribute('src');

			if (isInternational == "0")
			{
				if (SkypeActiveCallButtonPart == 0)    //left
				{
					//obj.firstChild.firstChild.setAttribute('src', '" + skype_sPathPrefix + "cb_down_l.gif');
					cb_part_l.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_down_l.gif')";
					if (cb_part_l.isSameNode(cb_part_ml) != true)
						cb_part_ml.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_down_m.gif')";
				}
				else                            //right
				{
					//obj.firstChild.firstChild.setAttribute('src', '" + skype_sPathPrefix + "cb_down_l.gif');
					cb_part_l.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_down_l.gif')";
					if (cb_part_l.isSameNode(cb_part_ml) != true)
						cb_part_ml.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_down_m.gif')";
					cb_part_mr.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_down_m.gif')";
					//obj.lastChild.lastChild.setAttribute('src', '" + skype_sPathPrefix + "cb_down_r"+(isFax?"_fax":"")+".gif');
					cb_part_r.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_down_r"+(isFax?"_fax":"")+".gif')";
				}
			}
			else
			{
				//obj.firstChild.firstChild.setAttribute('src', '" + skype_sPathPrefix + "cb_down_l.gif');
				cb_part_l.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_down_l.gif')";
				if (cb_part_l.isSameNode(cb_part_ml) != true)
					cb_part_ml.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_down_m.gif')";
				cb_part_mr.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_down_m.gif')";
					//obj.lastChild.lastChild.setAttribute('src', '" + skype_sPathPrefix + "cb_down_r"+(isFax?"_fax":"")+".gif');
				cb_part_r.style.backgroundImage = "url('" + skype_sPathPrefix + "cb_down_r"+(isFax?"_fax":"")+".gif')";
			}
		}
		else
		{
			//obj.firstChild.firstChild.setAttribute('src', skype_cb_l);
			cb_part_l.style.backgroundImage = skype_cb_l;
			if (cb_part_l.isSameNode(cb_part_ml) != true)
				cb_part_ml.style.backgroundImage = skype_cb_m;
			cb_part_mr.style.backgroundImage = skype_cb_m;
			//obj.lastChild.lastChild.setAttribute('src', skype_cb_r);
			cb_part_r.style.backgroundImage = skype_cb_r;
		}
	}
}

function __skype_nh_icon_mouseOver(obj)
{
    //obj.setAttribute("src", getArrowedIcon(obj.getAttribute('src')));

	var coords = getElementAbsolutePos(obj);
	if (timerID) clearTimeout(timerID);
	timerID = null;

	if (coords.x == lastX && coords.y == lastY) return;
	__skype_nh_icon_mouseOut1();
	lastX = coords.x;
	lastY = coords.y;

    var arrowImg = document.createElement("img");
    if(arrowImg)
    {
        arrowImg.id = "skype_arrow";
        //arrowImg.setAttribute('class', 'skype_name_highlight_arrow');
        
		arrowImg.setAttribute('src',"chrome://skype_ff_toolbar_win/content/icons/arrow.gif");        
        arrowImg.style.position = "absolute";
        arrowImg.style.left = (coords.x + 12) + "px";
        arrowImg.style.top = (coords.y) + "px";
        arrowImg.setAttribute("onmouseover", "__skype_nh_icon_mouseOut2();");
        arrowImg.setAttribute("onmouseout", "__skype_nh_icon_mouseOut();");
        document.body.appendChild(arrowImg);

    }
}

//returns the absolute position of some element within document
function getElementAbsolutePos(element)
{
    var res = new Object();
    res.x = 0; res.y = 0;
    if (element !== null)
    {
        res.x = element.offsetLeft;
        res.y = element.offsetTop;

        var offsetParent = element.offsetParent;
        var parentNode = element.parentNode;

        while (offsetParent !== null)
        {
            res.x += offsetParent.offsetLeft;
            res.y += offsetParent.offsetTop;

            if (offsetParent != document.body && offsetParent != document.documentElement)
            {
                res.x -= offsetParent.scrollLeft;
                res.y -= offsetParent.scrollTop;
            }
            //next lines are necessary to support FireFox problem with offsetParent  
            {
                while (offsetParent != parentNode && parentNode !== null)
                {
                    res.x -= parentNode.scrollLeft;
                    res.y -= parentNode.scrollTop;

                    parentNode = parentNode.parentNode;
                }
            }
            parentNode = offsetParent.parentNode;
            offsetParent = offsetParent.offsetParent;
        }
    }
    return res;
};


var timerID = null;
var lastX = 0;
var lastY = 0;

function __skype_nh_icon_mouseOut(obj)
{
	timerID = setTimeout(__skype_nh_icon_mouseOut1, 10);
}

function __skype_nh_icon_mouseOut2()
{
	if (timerID) clearTimeout(timerID);
	timerID = null;
}

function __skype_nh_icon_mouseOut1()
{
	var arrow = document.getElementById("skype_arrow");
    if(arrow)
    	document.body.removeChild(arrow);
    lastX = 0;
    lastY = 0;
}