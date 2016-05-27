XSLTea
===

XSLT implementation written in Xquery

To install in eXist-db:
--------------------

Download and install eXist-db 3 at http://exist-db.org

Build the package and install into eXist using the manager in the dashboard.

--------

XSLTea implements common XSLT functions in Xquery for your convenience. It's inspired by the great work of Michael Kay, the king of XSLT and fervent admirer of tea.

Since XSLT presupposes a context, you have to pass it around yourself in Xquery. To create a context, simply call the function `xsltea:create-context($root-node)`.

Now you can start adding templates to the context by calling `xsltea:template($context,$match,$function,$priority,$mode)`, where the provided function will be executed with the context and the current match when it has been selected by the processor. The run the processor, call `xsltea:apply-templates($context)`.

There's also a function to convert template rules from an XSLT document into XQuery-based rules, which can be simply called with `xsltea:transform($root-node,$xslt-node,$parameters)`, where $parameters is a key-value map.


Production Note
---------------
This library is currently in alpha, and not ready for production. If you care about this library, please help to improve it.

TODO:

* imports
* rigorous testing
* error handling
* anything else

Legacy Example
---------------

```xquery
xquery version "3.1";

import module namespace xsltea="http://lagua.nl/xquery/xsltea";

let $xsl := <xsl:stylesheet version="3.1"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="/">
        <xsl:variable name="test" select="a"/>
        <div>
            <xsl:value-of select="$test"/>
        </div>
    </xsl:template>
</xsl:stylesheet>

return xsltea:transform(element root { element a { "test" }},$xsl)
```


Pure XQuery Example
-------------------

```xquery
xquery version "3.1";

import module namespace xsltea="http://lagua.nl/xquery/xsltea";

let $c := xsltea:create-context(element root { element a { "test" }})
let $c := xsltea:template($c,"/",function($c,$n){
	let $test := $n/a
    return element div {
        string($test)
    }
})

return xsltea:apply-templates($c)
```
