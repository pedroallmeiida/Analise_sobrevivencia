---
title: "Análise de sobrevivência"
author: "Professor: Pedro Monteiro de Almeida Junior"
format: html
editor: visual
---


# Conteúdo programático

1.  **Conceitos básicos em análise de dados de sobrevivência**
    -   Tempos de falha e censura

    -   Exemplos de uso da análise de sobrevivência

    -   Representação de dados de sobrevivência

    -   Funções de interesse e suas relações
2.  **Técnicas não paramétricas para análise de sobrevivência**
    -   Estimador não paramétrico de Kaplan-Meier

    -   Comparação entre curvas de sobrevivência
3.  **Métodos paramétricos para análise de sobrevivência**
    -   Distribuições básicas em sobrevivência (Exponencial, Weibull e lognormal)

    -   Procedimentos para a escolha de uma distribuição

    -   Exemplos práticos no R
4.  **Modelos de regressão para a análise de dados de sobrevivência**
    -   Modelo de regressão exponencial

    -   Modelo de regressão Weibull

    -   Modelo de regressão Lognormal

    -   Análise de Resíduos

    -   Exemplos práticos no R
5.  **Modelo de riscos proporcionais de Cox**
    -   Modelo de Cox

    -   Estimação e interpretação dos parâmetros

    -   Análise de resíduos

    -   Exemplos práticos no R

# Referência bibliográfica principal

\[1\] E. A. Colosimo, S. R. Giolo, ANÁLISE DE SOBREVIVÊNCIA APLICADA , Edgard Blucher, São Paulo, 2006.

![](images/livro_imagem.jpg){fig-align="center"}

# Introdução a análise de sobrevivência

O pontapé inicial da análise de sobrevivência foi na área da saúde. Entretanto, ela se aplica em diversas outras áreas de aplicação. É comum se deparar com situações que é necessário obter o tempo até a ocorrência de determinado evento. Por exemplo:

-   Tempo até a cura de um paciente

-   Tempo até a inadimplência de um cliente

-   Tempo até a ocorrência de fraude

-   etc ...

Dessa forma, conseguimos determinar a sobrevida de um paciente, produto, clientes, etc... Aqui no curso, o objetivo é avaliar as principais técnicas de análise de sobrevivência e aplicá-las em diversas áres do nosso conhecimento. A sequir, vamos ver os principais conceitos em sobrevivência.

# CONCEITOS INICIAIS

Em análise de sobrevivência, a nossa variável de interesse é o tempo até a ocorrência de um determinado evento. Esse tempo é o que chamamos de **tempo de falha**. Na área da saúde por exemplo, o tempo de falha poderia ser: o [tempo até a cura]{.underline} de um paciente, [tempo até a reincidência]{.underline} de uma doença ou [tempo até o falecimento]{.underline} do paciente.

Os dados para análise de sobrevivência envolvem uma resposta que é avaliada sobre um período de tempo, que na maioria das vezes será difícil acompanhar todas as observações até a ocorrência do evento. Dessa forma teremos no estudo a presença de observações incompletas. Daí vem o conceito de censura nos dados.

**Censura:** a principal característica de dados de sobrevivência é a presença de censura, que é a observação parcial da resposta. Por exemplo, em algumas situações, o acompanhamento do indivíduo presente no estudo é interrompido ( o indivíduo mudou de cidade, faleceu, o estudo finalizou antes de ocorrer o evento desejado, etc...).

**Observação:** Sem a presença de censura, as técnicas estatísticas clássicas, como análise de regressão e análise de experimento poderiam ser usadas na análise desse tipo de dado. Por exemplo:

*Suponha que o interesse seja o de comparar o tempo médio de vida de três grupos de pacientes. Se não houver censuras, podemos usar a análise de variância para fazer a comparação entre os grupos de pacientes usando o tempo médio de vida. Entretanto, se houver censuras, não teremos a informações de pacientes que não concluíram o estudo. Portanto o uso de técnicas de análise de sobrevivência se torna indispensável nesses casos.*

# Caracterizando dados de sobrevivência

Os dados de sobrevivência são caracterizados por: (i) tempos de falha e (ii) censuras. Estas duas características constituem a nossa variável resposta (ou de interesse). A seguir vamos discutir essas características individualmente.

## Tempo de Falha

[Tempo de Sobrevivência ou Falha]{.underline}: Tempo até a ocorrência do evento de interesse.

O tempo de falha é constituído por três elementos:

[Elementos que definem o tempo de falha:]{.underline}

1.  A escala de medida

2.  O tempo inicial

3.  O evento de interesse

O tempo de sobrevivência vai do tempo inicial até a ocorrência do evento de interesse usando a escala de medida definida.

## Censura

É importante ressaltar que mesmo sendo incompletas as observações censuradas nos fornecem informações sobre os tempos de falha. A omissão dessas observações no cálculo das estatísticas de interesse pode acarretar em conclusões viciadas.

### TIPOS DE CENSURA:

1.  **Censura à direita:** O tempo de falha é superior ao tempo registrado. Desprezar essa informação faria com que o risco de ocorrência do evento de interesse fosse superestimado pois o tempo até a falha é desconhecido mas o evento de interesse não ocorreu até o último momento observado. Existem três conhecidos mecanismos de censura à direita.

2.  **Censura do Tipo I:** O estudo será terminado após um período préestabelecido de tempo. As observações cujo evento de interesse não foi observado até este tempo são ditas censuradas.

3.  **Censura do Tipo II:** O estudo será terminado após ter ocorrido o evento de interesse para um número pré-estabelecido de observações.

4.  **Censura Aleatória:** Ocorre se a observação for retirada no decorrer do estudo sem ter ocorrido o evento de interesse ou se o evento de interesse ocorrer por uma razão diferente da estudada.

5.  **Censura Intervalar:** ocorre quando o evento ocorreu em um certo intervalo. Isto é, o tempo exato da falha não é conhecido exatamente mas pertence a um intervalo. Ocorre por exemplo em estudos em que pacientes são acompanhados em visitas periódicas.

    **Ilustração dos tipos de censura:**

![Fonte: Colosimo & Giolo (2006)](images/exemplos_censura.png){fig-align="center"}

# Representação dos dados de sobrevivência

Os dados de sobrevivência para o indivíduo i $( i = 1, \ldots, n)$ sob estudo são representados, em geral, pelo par $(T_i,C_i)$, sendo $T_i$ o tempo de falha ou censura e $C_i$ a variável indicadora de falha ou censura, isto é,


$$
C_i = \left\{ \begin{array}{ll}
1,& \text{se } T_i \text{ é um tempo de falha} \\
0,& \text{se } T_i \text{ é um tempo censurado}
\end{array} \right.
$$


Assim, a variável de interesse em análise de sobrevivência é representada por duas colunas ( Tempo até a falha e censura).

## Estrutura dos banco de dados de sobrevivência

| Indivíduo (i) | Tempo (T_i) | Censura (C_i) | covariável (X_i) |
|---------------|-------------|---------------|------------------|
| 1             | 10          | 1             | 5                |
| 2             | 15          | 0             | 6                |
| 3             | 30          | 1             | 8                |

## Exemplos de Dados de Sobrevivência

Em diversas situações podemos usar técnicas de análise de sobrevivência. A área de maior destaque para essa técnica é a área médica. Podemos usar a análise de sobrevivência para identificar fatores de prognóstico para uma doença, comparação de tratamentos (uma prática bastante comum na área de oncologia, no qual podemos testar se um novo tratamento é melhor que o tratamento usual, ou seja, podemos avaliar se o tempo de sobrevivência de pacientes que aderiram ao novo tratamento é maior). A seguir vamos apresentar alguns exemplos da aplicação de sobrevivência.

### 1. Dados de Hepatite

Um estudo clínico aleatorizado foi realizado para investigar o efeito da terapia com esteróide no tratamento de hepatite viral aguda. 29 pacientes com esta doença foram aleatorizados para receber um placebo ou o tratamento com esteróide. Cada paciente foi acompanhado por 16 semanas ou até a morte (evento de interesse) ou até a perda de acompanhamento. Os tempos de sobrevivência observados, em semanas, para os dois grupos são apresentados são dados a seguir:

| [Grupos]{.smallcaps}    | [Tempo]{.smallcaps} | [Censura]{.smallcaps} |
|-------------------------|---------------------|-----------------------|
| [Controle]{.smallcaps}  | [1]{.smallcaps}     | [0]{.smallcaps}       |
| [Controle]{.smallcaps}  | [2]{.smallcaps}     | [0]{.smallcaps}       |
| [Controle]{.smallcaps}  | [3]{.smallcaps}     | [1]{.smallcaps}       |
| [Controle]{.smallcaps}  | [3]{.smallcaps}     | [1]{.smallcaps}       |
| [Controle]{.smallcaps}  | [3]{.smallcaps}     | [0]{.smallcaps}       |
| [Controle]{.smallcaps}  | [5]{.smallcaps}     | [0]{.smallcaps}       |
| [Controle]{.smallcaps}  | [5]{.smallcaps}     | [0]{.smallcaps}       |
| [Controle]{.smallcaps}  | [16]{.smallcaps}    | [1]{.smallcaps}       |
| [Controle]{.smallcaps}  | [16]{.smallcaps}    | [1]{.smallcaps}       |
| [Controle]{.smallcaps}  | [16]{.smallcaps}    | [1]{.smallcaps}       |
| [Controle]{.smallcaps}  | [16]{.smallcaps}    | [1]{.smallcaps}       |
| [Controle]{.smallcaps}  | [16]{.smallcaps}    | [1]{.smallcaps}       |
| [Controle]{.smallcaps}  | [16]{.smallcaps}    | [1]{.smallcaps}       |
| [Controle]{.smallcaps}  | [16]{.smallcaps}    | [1]{.smallcaps}       |
| [Controle]{.smallcaps}  | [16]{.smallcaps}    | [1]{.smallcaps}       |
| [Esteróide]{.smallcaps} | [1]{.smallcaps}     | [1]{.smallcaps}       |
| [Esteróide]{.smallcaps} | [1]{.smallcaps}     | [1]{.smallcaps}       |
| [Esteróide]{.smallcaps} | [1]{.smallcaps}     | [1]{.smallcaps}       |
| [Esteróide]{.smallcaps} | [1]{.smallcaps}     | [0]{.smallcaps}       |
| [Esteróide]{.smallcaps} | [4]{.smallcaps}     | [0]{.smallcaps}       |
| [Esteróide]{.smallcaps} | [5]{.smallcaps}     | [1]{.smallcaps}       |
| [Esteróide]{.smallcaps} | [7]{.smallcaps}     | [1]{.smallcaps}       |
| [Esteróide]{.smallcaps} | [8]{.smallcaps}     | [1]{.smallcaps}       |
| [Esteróide]{.smallcaps} | [10]{.smallcaps}    | [1]{.smallcaps}       |
| [Esteróide]{.smallcaps} | [10]{.smallcaps}    | [0]{.smallcaps}       |
| [Esteróide]{.smallcaps} | [12]{.smallcaps}    | [0]{.smallcaps}       |
| [Esteróide]{.smallcaps} | [16]{.smallcaps}    | [1]{.smallcaps}       |
| [Esteróide]{.smallcaps} | [16]{.smallcaps}    | [1]{.smallcaps}       |
| [Esteróide]{.smallcaps} | [16]{.smallcaps}    | [1]{.smallcaps}       |

: Tabela: Tempos em dia observados no estudo da hepatite

### Outros exemplos:

1.  *Em experimentos padrões na investigação de substâncias cancerígenas, animais em laboratórios são sujeitos a doses de substâncias e então são observados para ver se eles desenvolvem tumores. A principal variável de interesse pode ser o tempo até a ocorrência de tumores ou o tempo até a morte.*
2.  *Itens manufaturados de componentes eletrônicos são frequentemente sujeitos a testes de vida para obter informações sobre sua duração. Em laboratórios os itens são observados até a falha.*
3.  em análise de crédito, podemos estudar o comportamento de clientes para identificar fatores que impactam o tempo até a primeira inadimplência.

## Funções usadas para especificar o tempo de sobrevivência

O tempo de sobrevivência é uma variável aleatória T, contínua e positiva. Sendo assim, podemos atribuir alguma distribuição de probabilidade para T que pode ser especificada por algumas funções usadas na análise de sobrevivência, dentre as principais estão a função de sobrevivência e taxa de falha. A seguir, vamos apresentar algumas dessas funções.

### 1. Função de densidade

No contexto de sobrevivência a função $f(t)$ pode ser interpretada como limite da probabilidade de um indivíduo sofrer um evento em um pequeno intervalo por unidade de tempo.

Esta função é definida por


$$
f(t)=\lim _{\Delta t \rightarrow 0} \frac{P(t<T<t+\Delta t)}{\Delta t}
$$


Na **ausência de censuras** a f.d.p. pode ser estimada como a proporção de indivíduos que falham em um intervalo por unidade de tempo.


$$
\hat{f}(t)=\frac{\text{ Número de ocorrências em }(t, t+\Delta t)}{\left(\text{Número total de ocorrências }\right) \times(\Delta t)}
$$


### 2. Função de sobrevivência

A **função de sobrevivência** é uma das mais importantes e usadas funções em análise de sobrevivência. A função de sobrevivência é definida como a probabilidade de uma observação não falhar (sobreviver) até um certo tempo *t.* Em termos probabilísticos, isto é escrito como:


$$
S(t) = P(T \geq t). 
$$


Dessa forma, a [distribuição acumulada]{.underline} é definida como:


$$
P(T \leq t) = 1 - S(t)
$$


ou seja,a **distribuição acumulada** é a probabilidade de uma observação **não sobreviver** ao tempo *t*. Na Figura abaixo, temos um exemplo de comparação de curvas de sobrevivência entre dois grupos.

![Fonte: Colosimo & Giolo (2006)](images/curvas_sobrevivencia.png){fig-align="center"}

Como podemos ver, as curvas de sobrevivência para os dois grupos são diferentes. Em 10 anos, a taxa de sobrevivência para o grupo 1 é de aproximadamente 0.9, enquanto que para o grupo 2 é de 0.5.

Podemos estimar a curva de sobrevivência, quando [não há censuras]{.underline}, da seguinte forma:


$$
\hat S_x(t) = \frac{ \text{ Número de indivíduos sob risco no início do intervalo de tempo x }}{ \text{Número total de indivíduos} }
$$


### 3. Função de Taxa de Falha (ou risco):

A função taxa de falha é definida como a probabilidade de um indivíduo sofrer o evento entre o tempo $t$ e $t+\Delta t$, dado que ele sobreviveu até o tempo $t$.

Expressa o risco instantâneo de ocorrência de um evento em um pequeno intervalo de tempo, dado que até então o evento não tenha ocorrido.


$$
h(t)=\lim _{\Delta t \rightarrow 0} \frac{P(t \leq T<t+\Delta t \mid T \geq t)}{\Delta t}
$$


**IMPORTANTE:** h(t) é uma taxa e não uma probabilidade, pode assumir qualquer valor positivo.

Na ausência de censura a função de risco é estimada por


$$
\hat{h}(t)=\frac{N(t)}{R(t) \times \Delta t}
$$


$\mathrm{N}(\mathrm{t})$ : Número de eventos observados em cada intervalo de tempo (iniciando em t).

$R(t)$ : Número de observações sob risco no início do intervalo.

$\Delta t$ : amplitude do intervalo.

#### Comportamento da função de risco

A função de risco permite analisar o risco de um indivíduo sofrer um evento em um determinado tempo $t$, dado que ele já sobreviveu até aquele momento. Por exemplo, qual é o risco de um cliente se tornar inadimplente logo após a liberação de um crédito ? Será que o risco é constante ao longo do tempo ? É provável que não. Dessa forma, podemos ter vários tipos de comportamento de risco.

![Fonte: Carvalho et al. (2019)](images/exemplos_riscos.png){alt="Fonte: Carvalho et al. (2019)"}

### Função de risco (falha) acumulado

A função de risco acumulado $\Lambda(t)$ mede o risco de ocorrênia do evento até um determinado tempo $t$. Matematicamente, isso significa a soma de todos os riscos em todos os tempos até o tempo $t$. Como o tempo é uma variável aleatória contínua, $T$, o risco acumulado é dado por:


$$
\Lambda(t)=\int_0^t \lambda(u) d(u) 
$$


### Relações entre as funções

Para $T$ uma variável aleatória contínua e não negativa, tem-se, em termos das funções definidas anteriormente, algumas relações matemáticas são estabelecidas:


$$
\begin{align}
&\lambda(t) = \frac{f(t)}{S(t)} = - \frac{d}{d t}[\log S(t)], \\ \\
&\Lambda(t) = - \log S(t), \\ \\
&S(t) = \exp\{ - \Lambda(t) \}
\end{align}
$$ Portanto, com o conhecimento de uma das funções, por exemplo $S(t)$, implica no conhecimento das demais funções, isto é, $F(t)$, $f(t)$, $\lambda(t)$ e $\Lambda(t)$.


### Exemplo no R


::: {.cell}

```{.r .cell-code}
library(magrittr)
library(dplyr)
```
:::

::: {.cell}

```{.r .cell-code}
### Criando o dataset
Grupos = c( rep( "Controle", 15 ), rep( "Esteroide", 14 )  )
Tempo = c( 1,2,3,3,3,5,5,16, 16,16,16,16,16,16,16,1,1,1,1,4,5,7,8,10,10,12,16,16,16  )
Censura = c( 0,0,1,1,0,0,0, rep(1, 11), 0,0,1,1,1,1,0,0,1,1,1  )

dados = data.frame( Grupos, Tempo, Censura )

## Selecionando apenas dados sem censura

dados = dados %>% filter( Censura == 1 )
```
:::


### Exercícios

1.  Um grande número de indivíduos foi acompanhado para estudar o aparecimento de um certo sintoma. Os indivíduos foram incluídos ao longo do estudo e foi considerado como resposta de interesse a idade em que este sintoma apareceu pela primeira vez. Para os seis indivíduos selecionados e descritos a seguir, identifique o tipo de censura apresentado.

<!-- -->

(a) O primeiro indivíduo entrou no estudo com 25 anos já apresentando o sintoma.

(b) Outros dois indivíduos entraram no estudo com 20 e 28 anos e não apresentaram o sintoma até o encerramento do estudo.

(c) Outros dois indivíduos entraram com 35 e 40 anos e apresentaram o sintoma no segundo e no sexto exames respectivamente, após terem entrado no estudo. Os exames foram realizados a cada dois anos.

(d) O último indivíduo selecionado entrou no estudo com 36 anos e mudou da cidade depois de 4 anos sem ter apresentado o sintoma.

### Técnicas não paramétricas em sobrevivência

Anteriormente, vimos como estimar a função de sobrevivência e as demais quando não existe a presença de censura. Entretanto, na grande maioria dos casos vamos ter dados censurados, consequentemente teremos que usar técnicas que lidem com dados com censura. Nesta seção, vamos ver técnicas não paramétricas adequadas para esse tipo de situação.

O estimador Kaplan-Meier, também conhecido como estimador produto-limite, será utilizado para estimar a função de sobrevivência, $S(t)$, e o estimador de Nelson-Aalen para estimar a função de risco acumulado, $\Lambda(t)$. Na abordagem não paramétrica não é feita qualquer suposição sobre a distribuição probabilística do tempo de sobrevivência $T$, e assim não são estimados parâmetros estatísticos.

