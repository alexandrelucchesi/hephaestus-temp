<?php

function string_begins_with($string, $search) {
	return (strncmp($string, $search, strlen($search)) == 0);
}

function saveToFile($content, $file) {
	$open = fopen($file,"a");
	if ($open) {
		fwrite($open,$content);
		fclose($open);
	}
}

function getComponents($file) {
	$components = array();
	$lines = file ($file, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
	foreach($lines as $line) {
		$components[] = trim($line);
	}
	return $components;
}

function declarationsPVS($components) {
	$list = array();
	foreach($components as $component) $list[] = $component['name'];
	return implode(', ',$list) . ' : boolean'; 
}

function declarationsAlloy($components) {
	$list = array();
	foreach($components as $component) $list[] = $component['name'];
	return 'one sig ' . implode(', ',$list) . ' in Bool {}'; 
}


function getRequireds($path,$components,$compName) {
	$requireds = array();	
	$output="";
	
	$content = file_get_contents($path);
	
	//eliminar do array de componentes o próprio cara
	$key = array_search($compName, $components);
	unset($components[$key]);
	
	//pesquisar no texto, para cada componente, a presenca dele, caso positivo
	//adicione ao array de requireds
	foreach ($components as $component) {
		$pos = strpos($content, $component);

		if ($pos !== false) {
			$requireds[] = $component;
		}
	}
	/**/
	
	if (count($requireds)>0) return implode(',',$requireds);
	else return 'True';
}

function buildXMLfromCKarray($ck) {
	$header = '<configurationModel xmlns="ck.'.date('Ymd').'">';
	$xmlBody = '';
	foreach($ck as $featExp => $transformations) {
		$xmlBody.='<configuration xmlns="">
			';
		$xmlBody.='<expression>'.$featExp.'</expression>
			';
		foreach($transformations as $component=>$interface) {
			$xmlBody.='<transformation>
			<name>selectComponents</name>
			';
			$xmlBody.='<args>'.$component.'</args>
			';
			$xmlBody.='<required>'.$interface['required'].'</required>
			';
			$xmlBody.='<provided>'.$interface['provided'].'</provided>
			';
			$xmlBody.='</transformation>
		';
		}
		$xmlBody.='</configuration>
		';
	}
	$footer = '</configurationModel>';
	return $header.$xmlBody.$footer;
}

function buildFormulafromCKarray($ck) {
	$formula = '('.buildFormulaPVS($ck,'provided').') => ('.buildFormulaPVS($ck,'required').')';
	return $formula;
}

function buildFormulaPVS($ck,$type) {
	$formulaBody = '( ';
	$formulas = array();
	$formulaTemp = '';
	foreach($ck as $featExp => $transformations) {
		$formulaTemp = '( ('. $featExp . ') => ';
		$components = array();
		foreach($transformations as $component=>$interface) {
			$comps = str_replace(',',' AND ',$interface[$type]);
			$components[] = $comps;
		}
		$formulaTemp.= '('.implode(' AND ',$components).')';
		$formulaTemp.= ')';
		$formulas[] = $formulaTemp;
	}
	//print_r($formulas);
	$formulaBody.= implode(' AND ', $formulas);
	return $formulaBody . ' )';
}


function buildFormulaAlloy($ck,$type) {
	$formulaBody = 'pred '.$type.'[] { ';
	$formulas = array();
	$formulaTemp = '';
	$i=1;

	foreach($ck as $featExp => $transformations) {
		$formulaTemp = '( ('. $featExp . ') => ';
		$components = array();
		foreach($transformations as $component=>$interface) {
			$comps = explode(',',$interface[$type]);
			foreach($comps as $comp) {
				$components[] = $comp;
			}
			//echo "\n\n";			
		}
		
		$compAlloy = array();
		foreach($components as $comp) {
			//echo $comp."\n";
			$compAlloy[] = 'isTrue['.$comp.']';
		}
		$formulaTemp.= '('.implode(' and ',$compAlloy).')';
		$formulaTemp.= ')';
		$formulas[] = $formulaTemp;
	}
	//print_r($formulas);
	$formulaBody.= implode(' and ', $formulas);
	return $formulaBody . ' }';
}


function findexts ($filename) {
	$filename = strtolower($filename) ;
	$exts = split("[/\\.]", $filename) ;
	$n = count($exts)-1;
	$exts = $exts[$n];
	return $exts;
} 

function getDirectory( $path = '.', $level = 0 ){
	global $files;
    $ignore = array( 'cgi-bin', '.', '..' );
    // Directories to ignore when listing output. Many hosts
    // will deny PHP access to the cgi-bin.

    $dh = @opendir( $path );
    // Open the directory to the handle $dh
    
    while( false !== ( $file = readdir( $dh ) ) ){
    // Loop through the directory
    
        if( !in_array( $file, $ignore ) ){
        // Check that this file is not to be ignored
            
            $spaces = str_repeat( ' ', ( $level * 4 ) );
            // Just to add spacing to the list, to better
            // show the directory tree.
            
            if( is_dir( "$path/$file" ) ){
            // Its a directory, so we need to keep reading down...
            
                //echo "$spaces $file\n";
                getDirectory( "$path/$file", ($level+1) );
                // Re-call this same function but on a new directory.
                // this is what makes function recursive.
            
            } else {
            	$extension = findexts($file);
            	if ($extension == 'java' || $extension == 'aj') {
                	echo "$spaces $file";
                }
                // Just print out the filename
            
            }
        
        }
    
    }
    
    closedir( $dh );
    // Close the directory handle

}

function genFMmapCheck($qty) {
	$formula = '( ';
	$products = array();
	for ($i=1;$i<=$qty;$i++) {
		$products[] = 'product'.$i;
	}
	$formula.=implode(' OR ',$products);
	$formula.= ' ) <=> (semanticaFM)';
	return $formula;
}


function readPVSsubgoals($file, $features) {
		
	$lines = file ($file, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
	
	$subgoals = array();
	$subgoal = '';
	
	// Percorre o array, para montar o component model
	foreach ($lines as $line_num => $line) {
		$line = trim($line);
	    if (string_begins_with($line,'wellTypedSPL')) {
	    	$line = str_replace('wellTypedSPL.','',$line);
	    	$line = str_replace(' :','',$line);
	    	$subgoals[$line] = array();
	    	$subgoals[$line]['antecedent'] = array();
	    	$subgoals[$line]['antecedent']['features'] = array();
	    	$subgoals[$line]['antecedent']['components'] = array();
	    	$subgoals[$line]['consequent'] = array();
	    	$subgoals[$line]['consequent']['features'] = array();
	    	$subgoals[$line]['consequent']['components'] = array();
	    	$subgoal = $line;
	    	//echo "\n\n\n".$line."\n";
	    	$formulaPosition = 'antecedent';
	    }
	    else if (string_begins_with($line,'|-------')) {
	    	//echo "|-------\n";
	    	$formulaPosition = 'consequent';
	    }
	    else if (string_begins_with($line,'{')) {
	    	$line = str_replace('(','',$line);
	    	$line = str_replace(')','',$line);
	    	$line = trim(preg_replace('/\{.*\}/', '', $line));
			if (in_array($line,$features)) {
	    		$subgoals[$subgoal][$formulaPosition]['features'][] = $line;
	    	}
	    	else {
	    		$subgoals[$subgoal][$formulaPosition]['components'][] = $line;
	    	}
    		//echo $line."\n";
	    }
	    
	}

	return $subgoals;
}

function mapExpToAlloy($featExp,$featureList) {
	$vars = explode(' ', $featExp);
	$formula = '';
	foreach($vars as $var) {
		$var = trim($var);
		if (in_array($var,$featureList)) {
			$var = 'isTrue['.$var.']';
		}
		else $var = strtolower($var);
		$formula.= ' '.$var;
	}
	return trim($formula);
}

function getRequired($path) {
	$returnValues = array();
	$lines = file ($path, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
	foreach($lines as $line_num => $line) {
		if (string_begins_with($line,'public ')) {
			break;
		}
		else if (string_begins_with($line,'import br.ufpe')) {
			$component = explode('.',trim($line));
    		$compName = $component[(count($component)-1)];
			$returnValues[] = str_replace(';','',trim($compName));
		}
	}
	if (count($returnValues)>0)	return implode(',',$returnValues);
	else return 'True';
}


?>