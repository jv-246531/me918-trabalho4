
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Trabalho 4 - ME918

# Aplicativo Shiny para visualização de modelos de regressão linear múltipla

Este aplicativo Shiny realiza a construção de um modelo de regressão
linear com um conjunto de dados fornecido pelo usuário. Este conjunto de
dados deve, necessariamente, estar no formato `.csv`.

O usuário deve fazer o upload do arquivo `.csv` contendo os dados e, a
partir destes dados, selecionar variáveis para construir o modelo de
regressão, tanto as preditoras quanto a resposta (necessariamente
numérica). O aplicativo oferece visualizações gráficas do modelo e o
valor do coeficiente de determinação, $R^{2}$; além de apresentar a
tabela de Análise de Variância (ANOVA) para a regressão desenvolvida.

### Etapa 1: Carregando o banco de dados

É necessário fazer o upload de um arquivo `.csv` contendo os dados que
serão utilizados para construir o modelo de regressão. Nesta versão do
aplicativo, são aceitos apenas arquivos neste formato.

Para fazer o upload do arquivo desejado, clique no botão
`Escolher arquivo` e selecione-o.

### Etapa 2: Escolha das variáveis

Após o upload, o usuário deve selecionar a variável resposta e as
variáveis preditoras que serão usadas para estabelecer o modelo de
regressão. Para tanto, o aplicativo cria, de forma dinâmica, um painel
de escolha de variáveis, com base nas colunas do upload realizado.

- *Variável resposta*: O usuário deve escolher qual coluna será a
  variável resposta do modelo. Há a restrição de que só podem ser
  escolhidas variáveis numéricas.

- *Variáveis preditoras*: Qualquer outra variável, de caráter numérico
  ou categórico, em que há interesse de inserção ao modelo linear. Para
  bom comportamento, não é permitida a seleção da mesma variável
  resposta.

### Etapa 3: Construção do modelo

Após definição das variáveis, o usuário deve clicar no botão “Construir
Modelo” para ajustar o modelo de regressão linear.

O aplicativo contém uma opção de Bookmarking, gerando uma rota para
obter os resultados.

### Etapa 4: Visualização dos resultados

Uma vez que o modelo foi ajustado, o aplicativo exibirá o seguinte
conjunto de informações estatísticas.

- *Coeficiente de Determinação Ajustado*: A estatística
  $R^{2}_{\text{aj.}}=1-\dfrac{n-1}{n-p}\dfrac{SQE}{SQT}$ será exibida
  na tela, indicando a porcentagem da variabilidade dos dados que é
  explicada pelo modelo.

- *Tabela ANOVA*: A Análise de Variância (ANOVA) é um instrumento
  estatístico que apresenta, de modo geral, informações sobre a
  variabilidade do modelo de regressão, através de somas quadráticas
  parciais. Também indica o efeito que cada coeficiente tem quando
  adicionado sequencialmente na regressão produzida.

- *Análise exploratória*: São gerados três tipos de visualizações:

1)  “Valores Preditos x Resíduos”, em uma relação entre o que é esperado
    pelo modelo e o que foi visto, de fato, pelos dados. Caso o modelo
    seja adequado, espera-se uma associação aproximadamente linear;

2)  “Valores Preditos x Valores Observados”, em que podemos visualizar
    se suposições essenciais do modelo são satisfeitas: erro com
    distribuição normal e homoscedasticidade. Também é possível
    identificar se há observações . O resíduo calculado aqui é de
    caráter semi-studentizado.

3)  “Gráfico Quantil-Quantil Para Resíduos do Modelo”, em que há uma
    comparação entre os quantis empíricos da distribuição dos resíduos
    com quantis teóricos normais. Permite-se, novamente, verificar a
    suposição de normalidade do modelo.

O usuário pode se sentir livre para escolher sua cor favorita na
visualização dos gráficos indicados acima.

Para acesso via *shinyapps.io*, o usuário pode [clicar neste
link](https://8n7yli-jo0o-v0tor0vieira0de0castro.shinyapps.io/modelo_de_reg_lin_p_conj_de_dados_fornecido_pelo_usuario/).
