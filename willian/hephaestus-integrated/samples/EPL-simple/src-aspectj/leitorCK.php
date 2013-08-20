<?php
require_once './util.php';

$basepath = '/Users/leopoldoteixeira/Documents/CIn/workspaces/msc/ELP/src-aspectj/';
$pathCode = 'br/ufpe/cin/lps/elp/base/';

$componentList = getComponents($basepath.'componentList.txt');


$lines = file ('ELP.lst', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

$featureList = array('ELP', 'Print', 'Add', 'Sub', 'Div', 'Mul', 'Integer', 'Double', 'Value');


$comps = array();
$compNames = array();

// Percorre o array, para montar o component model
foreach ($lines as $line_num => $line) {
    if (string_begins_with($line,$pathCode)) {
    	$fullpath = trim($line);
    	$path = str_replace($pathCode,'',$fullpath);
    	$names = explode('/',$path);
    	$component = explode('.',$names[(count($names)-1)]);
    	$compName = trim($component[0]);
    	$comp = array('name'=>$compName,'path'=>$path,'fullpath'=>$fullpath);
    	$comps[] = $comp;
    	$compNames[] = $compName;
    	//echo $compName." => " . htmlspecialchars($path) . ";\n";
    }
}
/**/
//print_r($comps);

//echo declarationsAlloy($comps);
//echo declarationsPVS($comps);



$ck = array();
// Percorre o array, para montar o configuration model
foreach ($lines as $line_num => $line) {
	if (string_begins_with($line,'#')) {
		$featExp = trim(str_replace('#','',$line));
		//echo "\n\n".$featExp."\n";
		
		//FOR ALLOY USE THIS --> $featExp = mapToAlloy($featExp);
		//$featExp = mapExpToAlloy($featExp,$featureList);
		$ck[$featExp] = array();
	}
    else if (string_begins_with($line,$pathCode)) {
    	$path = trim($line);
    	$component = str_replace($pathCode,'',$path);
    	$names = explode('/',$component);
    	$component = explode('.',$names[(count($names)-1)]);
    	$compName = $component[0];
    	$ck[$featExp][$compName]['path'] = $path;
    	$ck[$featExp][$compName]['provided'] = $compName;
    	$ck[$featExp][$compName]['required'] = getRequireds($basepath.$path,$componentList,$compName);
    	
    	//echo $compName.",";
    }
}

echo "\n\n\n";
print_r($ck);
//$xmlCK = buildXMLfromCKarray($ck);
//saveToFile($xmlCK, 'ck.xml');


echo "\n\n\n";
//echo buildFormulaAlloy($ck,'provided');
//echo buildFormulaPVS($ck,'provided');
//echo "\n\n\n";
//echo buildFormulaAlloy($ck,'required');
//echo buildFormulaPVS($ck,'required');
echo "\n\n\n";
/**/

?>