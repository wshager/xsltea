xquery version "3.1";

import module namespace xsltea="http://lagua.nl/xquery/xsltea" at "xsltea.xqm";

let $xsl := <xsl:stylesheet version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="/">
        <xsl:variable name="test" select="a"/>
        <div>
            <xsl:value-of select="$test"/>
        </div>
    </xsl:template>
</xsl:stylesheet>


return xsltea:transform(element root { element a { "test" }},$xsl)
