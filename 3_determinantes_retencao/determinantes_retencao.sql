WITH 
    populacao
     AS(
        SELECT cod_regsaud, 
               populacao AS pop_censo22
        FROM "Analytics Layer".Territorial."População Censo 2022 por região de saúde"

    ),

    retencao_enfermeiro
     AS(
        SELECT  
            regiao_saude,  
            ROUND(retencao_geral,4) AS retencao
        FROM Dados.retencao."Enfermeiro_retencao_geral.parquet"
        
), 

    internacoes_aps 
    AS (
       SELECT 
            a.ANO, 
            b.uf_sigla,
            b.cod_regsaud,
            b.regiao_saude,
            SUM(a.TOTAL) AS internacoes_sensiveis_aps
        FROM "Analytics Layer"."Epidemiológico"."Internações por causas sensíveis à APS por município" a
        LEFT JOIN "Open Analytics Layer".Territorial."Hierarquia completa dos municípios" b
                ON a.MUNIC_RES = b.cod_municipio
        WHERE ANO = 2022
        GROUP BY 
                a.ANO, 
                b.uf_sigla,
                b.cod_regsaud,
                b.regiao_saude
    ), 

consulta_ies 
    AS(
        SELECT cod_regsaud, 
               regiao_saude,
               COUNT(DISTINCT(chave_ies)) AS total_ies_enfermagem
        FROM "Open Analytics Layer"."Educação"."Quantidade de vagas, matriculados, concluintes e inscritos em curso superior por instituição de ensino"
        WHERE ano = 2022 and curso = 'Enfermagem'
        GROUP BY cod_regsaud, 
                 regiao_saude
), 

consulta_vagas_ies 
    AS(
            SELECT 
                ano,
                uf_sigla,
                cod_regsaud,
                regiao_saude, 
                curso,
                SUM(qt_matricula_total) AS matriculas_ies,
                SUM(qt_ingresso_total) AS ingressos_ies,
                SUM(qt_vaga_total) AS vagas_ies,
                SUM(qt_inscrito_total) AS inscritos_ies
            FROM "Open Analytics Layer"."Educação"."Quantidade de vagas, matriculados, concluintes e inscritos em curso superior por instituição de ensino"
            WHERE ano = 2022 AND
                curso = 'Enfermagem'
            GROUP BY 
                ano,
                uf_sigla,
                cod_regsaud,
                regiao_saude, 
                curso 
), 

quant_leitos_uti
    AS(
        SELECT 
            cod_regsaud,
            SUM(QT_SUS) AS Leitos_UTI_SUS,
            SUM(QT_NSUS) AS Leitos_UTI_NSUS
        FROM dados.cnes.lt
        LEFT JOIN 
            "Open Analytics Layer".Territorial."Hierarquia completa dos municípios" B
            ON lt.CODUFMUN = CAST(B.cod_municipio AS CHARACTER)
        WHERE 
            substr(lt.COMPETEN, 5, 2) = '01' AND
            substr(lt.COMPETEN, 1, 4) = '2022' AND
            CODLEITO IN (61,62,63,74,75,76,77,78,79,80,81,82,83,85,86)
        GROUP BY 
            cod_regsaud

), 

quant_leitos_sus_n_sus
    AS(
           SELECT 
               ano,
               uf_sigla,
               cod_regsaud,
               regiao_saude, 
               SUM(quantidade_sus) AS quant_leitos_sus,
               SUM(quantidade_nao_sus) AS quant_leitos_n_sus
        FROM "Open Analytics Layer".Infraestrutura."Quantidade de leitos SUS e não SUS por município"
        WHERE ano = 2022 
        GROUP BY 
               ano,
               uf_sigla,
               cod_regsaud,
               regiao_saude
), 

estabelecimentos_tipo AS (

SELECT
    m.cod_regsaud,
    m.regiao_saude,
    TP_UNID AS tipo_de_unidade,
    UN.descricao_tratado AS descricao,
    COUNT(*) AS numero_estabelecimentos
 FROM
    dados.cnes.ST AS DS
LEFT JOIN
     "Open Analytics Layer".Territorial."Hierarquia completa dos municípios" AS m
            ON DS.CODUFMUN = m.cod_municipio
LEFT JOIN
    Dados.cnes."TP_UNID.parquet" UN
    ON DS.TP_UNID = UN."TIPO DE ESTABELECIMENTO"
WHERE
    COMPETEN = '202201'
GROUP BY
    m.cod_regsaud,
    m.regiao_saude,
    TP_UNID, 
    UN.descricao_tratado

),


Unidade_tipo_wide AS(
    
    SELECT
        cod_regsaud,
        regiao_saude,
        SUM(CASE WHEN descricao = 'Unidade Móvel Terrestre' THEN numero_estabelecimentos ELSE 0 END) AS Unidade_Movel_Terrestre,
        SUM(CASE WHEN descricao = 'Centro de Saúde/Unidade Básica' THEN numero_estabelecimentos ELSE 0 END) AS Centro_de_Saude_Unidade_Basica,
        SUM(CASE WHEN descricao = 'Oficina Ortopédica' THEN numero_estabelecimentos ELSE 0 END) AS Oficina_Ortopedica,
        SUM(CASE WHEN descricao = 'Central de Regulação de Serviços de Saúde' THEN numero_estabelecimentos ELSE 0 END) 
        AS Central_de_Regulacao_de_Servicos_de_Saude,
        SUM(CASE WHEN descricao = 'Unidade Mista' THEN numero_estabelecimentos ELSE 0 END) AS Unidade_Mista,
        SUM(CASE WHEN descricao = 'Unidade Móvel Fluvial' THEN numero_estabelecimentos ELSE 0 END) AS Unidade_Movel_Fluvial,
        SUM(CASE WHEN descricao = 'Unidade de Atenção à Saúde Indígena' THEN numero_estabelecimentos ELSE 0 END) AS Unidade_de_Atencao_a_Saude_Indigena,
        SUM(CASE WHEN descricao = 'Consultório Isolado' THEN numero_estabelecimentos ELSE 0 END) AS Consultorio_Isolado,
        SUM(CASE WHEN descricao = 'Centro de Parto Normal - Isolado' THEN numero_estabelecimentos ELSE 0 END) AS Centro_de_Parto_Normal_Isolado,
        SUM(CASE WHEN descricao = 'Hospital Geral' THEN numero_estabelecimentos ELSE 0 END) AS Hospital_Geral,
        SUM(CASE WHEN descricao = 'Central de Notificação, Captação e Distribuição de Órgãos Estadual' THEN numero_estabelecimentos ELSE 0 END) 
        AS Central_de_Notificacao_Captacao_e_Distribuicao_de_orgaos_Estadual,
        SUM(CASE WHEN descricao = 'Farmácia' THEN numero_estabelecimentos ELSE 0 END) AS Farmacia,
        SUM(CASE WHEN descricao = 'Serviço de Atenção Domiciliar Isolado (Home Care)' THEN numero_estabelecimentos ELSE 0 END) 
        AS Servico_de_Atencao_Domiciliar_Isolado_Home_Care,
        SUM(CASE WHEN descricao = 'Telessaúde' THEN numero_estabelecimentos ELSE 0 END) AS Telessaude,
        SUM(CASE WHEN descricao = 'Pronto-Socorro Geral' THEN numero_estabelecimentos ELSE 0 END) AS Pronto_Socorro_Geral,
        SUM(CASE WHEN descricao = 'Clínica/Centro de Especialidade' THEN numero_estabelecimentos ELSE 0 END) AS Clinica_Centro_de_Especialidade,
        SUM(CASE WHEN descricao = 'Central de Regulação do Acesso' THEN numero_estabelecimentos ELSE 0 END) AS Central_de_Regulacao_do_Acesso,
        SUM(CASE WHEN descricao = 'Pronto-Socorro Especializado' THEN numero_estabelecimentos ELSE 0 END) AS Pronto_Socorro_Especializado,
        SUM(CASE WHEN descricao = 'Posto de Saúde' THEN numero_estabelecimentos ELSE 0 END) AS Posto_de_Saude,
        SUM(CASE WHEN descricao = 'Pronto-Socorro Traumato-Ortorpédico (Antigo)' THEN numero_estabelecimentos ELSE 0 END) 
        AS Pronto_Socorro_Traumato_Ortorpedico_Antigo,
        SUM(CASE WHEN descricao = 'Unidade de Apoio Diagnóstico e Terapia (Sadt Isolado)' THEN numero_estabelecimentos ELSE 0 END) 
        AS Unidade_de_Apoio_Diagnostico_e_Terapia_Sadt_Isolado,
        SUM(CASE WHEN descricao = 'Centro de Atenção Psicossocial' THEN numero_estabelecimentos ELSE 0 END) AS Centro_de_Atencao_Psicossocial,
        SUM(CASE WHEN descricao = 'Policlínica' THEN numero_estabelecimentos ELSE 0 END) AS Policlinica,
        SUM(CASE WHEN descricao = 'Laboratório Central de Saúde Pública Lacen' THEN numero_estabelecimentos ELSE 0 END) 
        AS Laboratorio_Central_de_Saude_Publica_Lacen,
        SUM(CASE WHEN descricao = 'Polo de Prevenção de Doenças e Agravos e Promoção da Saúde' THEN numero_estabelecimentos ELSE 0 END) 
        AS Polo_de_Prevencao_de_Doencas_e_Agravos_e_Promocao_da_Saude,
        SUM(CASE WHEN descricao = 'Unidade de Vigilância Sanitária (Antigo)' THEN numero_estabelecimentos ELSE 0 END) 
        AS Unidade_de_Vigilância_Sanitaria_Antigo,
        SUM(CASE WHEN descricao = 'Hospital Especializado' THEN numero_estabelecimentos ELSE 0 END) AS Hospital_Especializado,
        SUM(CASE WHEN descricao = 'Unidade Móvel de Nivel Pré-Hospitalar na Área de Urgência' THEN numero_estabelecimentos ELSE 0 END) 
        AS Unidade_Movel_de_Nivel_Pre_Hospitalar_na_Area_de_Urgencia,
        SUM(CASE WHEN descricao = 'Unidade de Saúde da Família' THEN numero_estabelecimentos ELSE 0 END) AS Unidade_de_Saude_da_Familia,
        SUM(CASE WHEN descricao = 'Unidade de Vigilância Em Saúde' THEN numero_estabelecimentos ELSE 0 END) AS Unidade_de_Vigilancia_Em_Saude,
        SUM(CASE WHEN descricao = 'Pronto-Socorro de Hospital Geral (Antigo)' THEN numero_estabelecimentos ELSE 0 END) 
        AS Pronto_Socorro_de_Hospital_Geral_Antigo,
        SUM(CASE WHEN descricao = 'Unidade de Atenção em Regime Residencial' THEN numero_estabelecimentos ELSE 0 END) 
        AS Unidade_de_Atencao_em_Regime_Residencial,
        SUM(CASE WHEN descricao = 'Hospital/Dia - Isolado' THEN numero_estabelecimentos ELSE 0 END) AS Hospital_Dia_Isolado,
        SUM(CASE WHEN descricao = 'Laboratório de Saúde Pública' THEN numero_estabelecimentos ELSE 0 END) AS Laboratorio_de_Saude_Publica,
        SUM(CASE WHEN descricao = 'Centro de Apoio à Saúde da Família' THEN numero_estabelecimentos ELSE 0 END) AS Centro_de_Apoio_a_Saude_da_Familia,
        SUM(CASE WHEN descricao = 'Central de Gestão em Saúde' THEN numero_estabelecimentos ELSE 0 END) AS Central_de_Gestao_em_Saude,
        SUM(CASE WHEN descricao = 'Pronto Atendimento' THEN numero_estabelecimentos ELSE 0 END) AS Pronto_Atendimento,
        SUM(CASE WHEN descricao = 'Polo Academia da Saúde' THEN numero_estabelecimentos ELSE 0 END) AS Polo_Academia_da_Saude,
        SUM(CASE WHEN descricao = 'Central de Regulação Médica das Urgências' THEN numero_estabelecimentos ELSE 0 END) 
        AS Central_de_Regulacao_Medica_das_Urgencias,
        SUM(CASE WHEN descricao = 'Cooperativa ou Empresa de Cessão de Trabalhadores na Saúde' THEN numero_estabelecimentos ELSE 0 END) 
        AS Cooperativa_ou_Empresa_de_Cessao_de_Trabalhadores_na_Saude,
        SUM(CASE WHEN descricao = 'Centro de Atenção Hemoterapia e ou Hematológica' THEN numero_estabelecimentos ELSE 0 END) 
        AS Centro_de_Atencao_Hemoterapia_e_ou_Hematológica
    FROM estabelecimentos_tipo
    GROUP BY  
        cod_regsaud,
        regiao_saude

), 

total_enfermeiros     
    AS( 
        SELECT 
        B.regiao, 
        B.uf_sigla, 
        B.cod_macrorregiao,
        B.macrorregiao,
        B.cod_regsaud, 
        B.regiao_saude,   
        substr(A.COMPETEN, 1, 4) AS ano,
        CASE
            WHEN LENGTH(A.codufmun) = 7 THEN SUBSTR(A.codufmun, 1, 6)
            WHEN A.codufmun LIKE '53%' THEN '530010' 
            ELSE A.codufmun
        END AS cod_ibge,
               A.uf, 
               A.CPF_PROF,
               A.CBO
        FROM Dados.cnes.PF A
        LEFT JOIN "Open Analytics Layer".Territorial."Hierarquia completa dos municípios" B
                 ON A.codufmun = CAST(B.cod_municipio AS CHARACTER)
        WHERE 
            A.COMPETEN = '202201' AND
            substr(A.CBO, 1, 4) = '2235' 
           
            
), 

total_enfermeiros_rs 
AS(

SELECT cod_regsaud, 
       COUNT(DISTINCT(CPF_PROF)) AS TOTAL_PROF
FROM total_enfermeiros
GROUP BY cod_regsaud

), 

CNES_VINC 
    AS (
        SELECT 
            SUBSTR(PF.COMPETEN, 1, 4) AS ano, 
            a.regiao, 
            a.uf_sigla, 
            a.cod_macrorregiao,
            a.macrorregiao,
            a.cod_regsaud, 
            a.regiao_saude, 
            CASE
                WHEN LENGTH(PF.CODUFMUN) = 7 THEN SUBSTR(PF.CODUFMUN, 1, 6)
                WHEN PF.CODUFMUN LIKE '53%' THEN '530010'
                ELSE PF.CODUFMUN
            END AS cod_ibge, 
            a.municipio,
            a.latitude, 
            a.longitude,
            PF.CBO, 
            PF.VINCULAC, 
            CASE
                WHEN SUBSTR(VINCULAC, 1, 4) = '0101' OR
                     SUBSTR(VINCULAC, 1, 4) = '0102' OR 
                     SUBSTR(VINCULAC, 1, 4) = '0105' OR
                     SUBSTR(VINCULAC, 1, 3) = '100'  THEN 'Protegido'
                WHEN 
                     SUBSTR(VINCULAC, 1, 4) = '0103' OR
                     SUBSTR(VINCULAC, 1, 4) = '0104' OR 
                     SUBSTR(VINCULAC, 1, 2) = '02' OR
                     SUBSTR(VINCULAC, 1, 2) = '03' OR
                     SUBSTR(VINCULAC, 1, 4) = '0401' OR
                     SUBSTR(VINCULAC, 1, 4) = '0402' OR
                     SUBSTR(VINCULAC, 1, 2) = '07' OR
                     SUBSTR(VINCULAC, 1, 2) = '08' OR 
                     SUBSTR(VINCULAC, 1, 2) = '09' THEN 'Precarizado'
                WHEN 
                     SUBSTR(VINCULAC, 1, 2) = '05' OR 
                     SUBSTR(VINCULAC, 1, 2) = '06' OR 
                     SUBSTR(VINCULAC, 1, 4) = '0403' THEN 'Outros'
                ELSE 'Sem informação'
            END categorias_vinculos,
            v.DESCRIÇÃO
           FROM Dados.cnes.PF PF
        LEFT JOIN Dados.cnes."VINCULAC.parquet" v            
            ON PF.VINCULAC = v.CHAVE
        LEFT JOIN "Open Analytics Layer".Territorial."Hierarquia completa dos municípios" a 
            ON SUBSTR(PF.CODUFMUN, 1, 6) = CAST(a.cod_municipio AS CHARACTER)
        WHERE PF.COMPETEN = '202201' AND 
              substr(PF.CBO, 1, 4) = '2235'
        
    ), 

Percentuais_vinculos AS (

SELECT 
    ano,
    cod_regsaud, 
    regiao_saude,
    categorias_vinculos, 
    COUNT(*) as quantidade,
    ROUND((COUNT(*) * 100.0) / SUM(COUNT(*)) OVER (PARTITION BY    
                                                        cod_regsaud, 
                                                        regiao_saude),2) as percentual
FROM 
    CNES_VINC
GROUP BY 
    ano,
    cod_regsaud, 
    regiao_saude,
    categorias_vinculos
ORDER BY 
    ano,
    cod_regsaud

), 

precarizados AS(

SELECT * FROM Percentuais_vinculos
WHERE categorias_vinculos = 'Precarizado'), 

km AS(

    SELECT 
           cod_regsaud,
           regiao_saude,
           ROUND(AVG(km),4) AS dist_media_capital_km 

    FROM Dados.distancias."distancias_municipios_capitais.parquet" A
        LEFT JOIN "Open Analytics Layer".Territorial."Hierarquia completa dos municípios" B
                     ON A.cod_municipio = CAST(B.cod_municipio AS INTEGER)

    GROUP BY 
        cod_regsaud,
        regiao_saude

), 

percentual_meio_acesso AS (
    SELECT 
        B.cod_regsaud, 
        B.regiao_saude,
        A.acesso_rodovia, 
        COUNT(*) as quantidade,
        ROUND((COUNT(*) * 100.0) / SUM(COUNT(*)) OVER (PARTITION BY    
                                                            cod_regsaud, 
                                                            regiao_saude),2) as percentual_rodovias

    FROM Dados.distancias."distancias_municipios_capitais.parquet" A
        LEFT JOIN "Open Analytics Layer".Territorial."Hierarquia completa dos municípios" B
                        ON A.cod_municipio = CAST(B.cod_municipio AS INTEGER)
    GROUP BY 
        cod_regsaud, 
        regiao_saude,
        acesso_rodovia
), 

acesso_rodovias AS(

SELECT * FROM percentual_meio_acesso
WHERE acesso_rodovia = 'sim'

), 


homicidios AS(

SELECT cod_regsaud,
       SUM(obitos_ano) AS obitos_homicidios
FROM "Open Analytics Layer"."Epidemiológico".Mortalidade."Taxa de mortalidade por homicídios"
WHERE ano = '2022'
GROUP BY 
       cod_regsaud

), 


qtd_vinculos     
        AS( 
            SELECT substr(pf.COMPETEN, 1, 4) AS ano,
            B.cod_regsaud, 
            B.regiao_saude,
            CASE
                WHEN LENGTH(pf.codufmun) = 7 THEN SUBSTR(pf.codufmun, 1, 6)
                WHEN pf.codufmun LIKE '53%' THEN '530010' 
                ELSE pf.codufmun
            END AS cod_ibge,
                   PF.uf, 
                   PF.CPF_PROF
            FROM Dados.cnes.PF PF
            LEFT JOIN "Open Analytics Layer".Territorial."Hierarquia completa dos municípios" B
                  ON PF.codufmun = CAST(B.cod_municipio AS CHARACTER)
            WHERE COMPETEN = '202201' AND 
                  substr(PF.CBO, 1, 4) = '2235'   
        ),

contagem_vinculos
        AS(
            SELECT cod_regsaud, 
                   regiao_saude,
                   CPF_PROF, 
                   COUNT(*) as total
            FROM qtd_vinculos
            GROUP BY cod_regsaud, 
                     regiao_saude,
                     CPF_PROF 
        ),

media_vinculos 
        AS(
  SELECT  cod_regsaud, 
          regiao_saude,
          ROUND(AVG(total), 4) AS vinc_medio
  FROM contagem_vinculos
  GROUP BY cod_regsaud, 
           regiao_saude
           ), 


pib AS(

    SELECT B.cod_regsaud,
       B.regiao_saude, 
       ROUND(SUM(A.produto_interno_bruto_precos_correntes_1000),4) AS PIB_1000     
    FROM Dados.pib."pib_2010_2021.parquet" A
    LEFT JOIN "Analytics Layer".Territorial."Municípios - Hierarquia Completa" B
        ON A.codigo_municipio = B.cod_municipiodv
    WHERE ano = 2021
    GROUP BY B.cod_regsaud,
             B.regiao_saude

),

amazonia AS
( 
    SELECT 
        B.cod_regsaud, 
        B.regiao_saude,
        A.amazonia_legal, 
        COUNT(*) as quantidade,
        ROUND((COUNT(*) * 100.0) / SUM(COUNT(*)) OVER (PARTITION BY    
                                                            cod_regsaud, 
                                                            regiao_saude),2) as percentual_amazonia_legal

    FROM Dados.pib."pib_2010_2021.parquet" A
        LEFT JOIN "Open Analytics Layer".Territorial."Hierarquia completa dos municípios" B
                        ON A.codigo_municipio = B.cod_municipiodv
    WHERE ano = 2021
    GROUP BY 
        cod_regsaud, 
        regiao_saude,
        amazonia_legal
),

percentual_amazonia AS(

    SELECT * FROM amazonia
    WHERE amazonia_legal = 'Sim'


),

tbl_semiarido AS
( 
    SELECT 
        B.cod_regsaud, 
        B.regiao_saude,
        A.semiarido, 
        COUNT(*) as quantidade,
        ROUND((COUNT(*) * 100.0) / SUM(COUNT(*)) OVER (PARTITION BY    
                                                            cod_regsaud, 
                                                            regiao_saude),2) as percentual_semiarido

    FROM Dados.pib."pib_2010_2021.parquet" A
        LEFT JOIN "Open Analytics Layer".Territorial."Hierarquia completa dos municípios" B
                        ON A.codigo_municipio = B.cod_municipiodv
    WHERE ano = 2021
    GROUP BY 
        cod_regsaud, 
        regiao_saude,
        semiarido
),

percentual_semiarido AS(

    SELECT * FROM tbl_semiarido
    WHERE semiarido = 'Sim'

)




SELECT  c.uf_sigla,
        b.cod_regsaud,
        h.regiao_saude,
        a.retencao,
        b.pop_censo22,
        i.TOTAL_PROF,
        ROUND(CAST(i.TOTAL_PROF AS FLOAT)/CAST(b.pop_censo22 AS FLOAT) * 1000, 4) AS razao_enfermeiros_1000,
        c.internacoes_sensiveis_aps,
        d.total_ies_enfermagem,
        e.matriculas_ies,
        e.ingressos_ies,
        e.vagas_ies,
        e.inscritos_ies,
        f.Leitos_UTI_SUS,
        f.Leitos_UTI_NSUS,
        g.quant_leitos_sus,
        g.quant_leitos_n_sus,
        h.Unidade_Movel_Terrestre,
        h.Centro_de_Saude_Unidade_Basica,
        h.Oficina_Ortopedica,
        h.Central_de_Regulacao_de_Servicos_de_Saude,
        h.Unidade_Mista,
        h.Unidade_Movel_Fluvial,
        h.Unidade_de_Atencao_a_Saude_Indigena,
        h.Consultorio_Isolado,
        h.Centro_de_Parto_Normal_Isolado,
        h.Hospital_Geral,
        h.Central_de_Notificacao_Captacao_e_Distribuicao_de_orgaos_Estadual,
        h.Farmacia,
        h.Servico_de_Atencao_Domiciliar_Isolado_Home_Care,
        h.Telessaude,
        h.Pronto_Socorro_Geral,
        h.Clinica_Centro_de_Especialidade,
        h.Central_de_Regulacao_do_Acesso,
        h.Pronto_Socorro_Especializado,
        h.Posto_de_Saude,
        h.Pronto_Socorro_Traumato_Ortorpedico_Antigo,
        h.Unidade_de_Apoio_Diagnostico_e_Terapia_Sadt_Isolado,
        h.Centro_de_Atencao_Psicossocial,
        h.Policlinica,
        h.Laboratorio_Central_de_Saude_Publica_Lacen,
        h.Polo_de_Prevencao_de_Doencas_e_Agravos_e_Promocao_da_Saude,
        h.Unidade_de_Vigilância_Sanitaria_Antigo,
        h.Hospital_Especializado,
        h.Unidade_Movel_de_Nivel_Pre_Hospitalar_na_Area_de_Urgencia,
        h.Unidade_de_Saude_da_Familia,
        h.Unidade_de_Vigilancia_Em_Saude,
        h.Pronto_Socorro_de_Hospital_Geral_Antigo,
        h.Unidade_de_Atencao_em_Regime_Residencial,
        h.Hospital_Dia_Isolado,
        h.Laboratorio_de_Saude_Publica,
        h.Centro_de_Apoio_a_Saude_da_Familia,
        h.Central_de_Gestao_em_Saude,
        h.Pronto_Atendimento,
        h.Polo_Academia_da_Saude,
        h.Central_de_Regulacao_Medica_das_Urgencias,
        h.Cooperativa_ou_Empresa_de_Cessao_de_Trabalhadores_na_Saude,
        h.Centro_de_Atencao_Hemoterapia_e_ou_Hematológica,
        j.percentual AS percentual_precarizacao,
        CASE
            WHEN cod_regsaud = '53001' OR cod_regsaud = '35016' THEN 100.0
            ELSE m.percentual_rodovias
        END percentual_rodovias,
        CASE
            WHEN cod_regsaud = '53001' OR cod_regsaud = '35016' THEN 0
            ELSE l.dist_media_capital_km
        END distancia_media_capital_km,
        n.obitos_homicidios,
        (n.obitos_homicidios/b.pop_censo22) * 1000 AS obitos_1000_hab,
        o.vinc_medio,
        p.PIB_1000,
        (p.PIB_1000 * 1000)/b.pop_censo22 AS PIB_percapita,
        CASE
            WHEN q.percentual_amazonia_legal IS NULL THEN 0.0
            ELSE  q.percentual_amazonia_legal
        END AS percentual_amazonia_legal_corrigido,
        CASE
            WHEN r.percentual_semiarido IS NULL THEN 0.0
            ELSE  r.percentual_semiarido
        END AS percentual_semiarido_corrigido
FROM retencao_enfermeiro a
    LEFT JOIN populacao b
        ON a.regiao_saude = b.cod_regsaud
    LEFT JOIN internacoes_aps c
        ON a.regiao_saude = c.cod_regsaud
    LEFT JOIN consulta_ies d
        ON a.regiao_saude = d.cod_regsaud
    LEFT JOIN consulta_vagas_ies e
        ON a.regiao_saude = e.cod_regsaud
    LEFT JOIN quant_leitos_uti f 
        ON a.regiao_saude = f.cod_regsaud
    LEFT JOIN quant_leitos_sus_n_sus g
        ON a.regiao_saude = g.cod_regsaud
    LEFT JOIN Unidade_tipo_wide h
        ON a.regiao_saude = h.cod_regsaud
    LEFT JOIN total_enfermeiros_rs i
        ON a.regiao_saude = CAST(i.cod_regsaud AS INTEGER)
    LEFT JOIN precarizados j 
        ON a.regiao_saude = j.cod_regsaud
    LEFT JOIN km l
        ON a.regiao_saude = l.cod_regsaud 
    LEFT JOIN acesso_rodovias m
        ON a.regiao_saude = m.cod_regsaud
    LEFT JOIN homicidios n
        ON a.regiao_saude = n.cod_regsaud
    LEFT JOIN media_vinculos o
        ON a.regiao_saude = o.cod_regsaud
    LEFT JOIN pib p
        ON a.regiao_saude = p.cod_regsaud
    LEFT JOIN percentual_amazonia q
        ON a.regiao_saude = q.cod_regsaud
    LEFT JOIN percentual_semiarido r
        ON a.regiao_saude = r.cod_regsaud
