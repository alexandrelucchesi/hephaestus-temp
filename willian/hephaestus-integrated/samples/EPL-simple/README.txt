Properties e Subtipo (src)
Este projeto apenas demonstra como estender a base utilizada em classe com:
- as operações de multiplicação e divisão, utilizando polimorfismo de subtipo;
- possibilidade de trocar os idiomas usando arquivos de propriedades.

Polimorfismo Paramétrico (src-parametric)
Variações implementadas: Tipo
Neste exemplo, as classes trabalham em função do tipo. Ou seja, as operações (adição, multiplicação, etc...) dependem do tipo (integer ou double). Assim, as operações estão definidas dentro da declaração do tipo. Desse modo, as classes AddExp, SubExp, etc, apenas chamam as operações de um determinado tipo. É importante frisar que nesta solução, todas as classes são dependentes do tipo utilizado na aplicação (inteiro ou double). Este tipo é especificado uma única vez, quando o parser é instanciado (ver método main - age como mecanismo de decisão). 

Compilação Condicional (src-cc)
Variações implementadas: Print, MultDiv, Tipo
O arquivo build.xml age como mecanismo de decisão para geração de produtos, neste caso. As features são definidas na tag symbols. No arquivo de exemplo, estão definidos dois produtos. As tags poderiam ser lidas de um arquivo .symbols, também incluso no exemplo. 

AspectJ (src-aspectj)
Variações implementadas: Print, Tipo
Neste exemplo, partimos de uma base já definida com Integer e as operações de Multiplicação e Divisão já presentes. Sendo assim, os aspectos definem o novo tipo Double e as operações com este novo tipo, assim como a feature de impressão da expressão fornecida para cálculo. Para gerar produtos com/sem as features, é só incluir/remover os aspectos do build path do projeto.

Velocity (src-velocity)
Variações implementadas: Print, MultDiv, Tipo
O arquivo ExpressionControl.vm age como mecanismo de decisão. Basta mudar o valor das variáveis para gerar produtos diferentes. Para rodar o build.xml tem que adicionar o jar do velocity no classpath do ant (em preferências do eclipse - ant - runtime - global entries).

Extension Points
Variações implementadas: Idioma
O plugin ELP-EP é a base e o ELP-EP-BR define o idioma português. Para trocar o idioma, é necessário definir um novo plugin, fazendo o link com o ponto de extensão definido na base e associando a este um novo arquivo de propriedades. 