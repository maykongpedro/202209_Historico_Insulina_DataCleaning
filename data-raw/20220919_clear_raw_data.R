

# Read data ---------------------------------------------------------------

raw_data <- readr::read_table(
    file = "data-raw/conversa_whats.txt",
    col_names = FALSE
)


# Transform ---------------------------------------------------------------

# ajustar colunas e observações iniciais
tidied_data <- raw_data |>
    # ajustar nome das colunas
    dplyr::rename(
        data_mensagem = X1,
        horario_mensagem = X2,
        usuario = X4,
    ) |>
    dplyr::mutate(

        # converter tudo para caractere
        dplyr::across(
            .cols = dplyr::everything(),
            .fns = as.character
        ),

        # substituir NAs
        dplyr::across(
            .cols = dplyr::everything(),
            .fns = tidyr::replace_na,
            replace = " "
        ),

        # ajustar tipo de dados
        data_mensagem = lubridate::dmy(data_mensagem)

    ) |>
    # juntar mensagem completa
    tidyr::unite(
        col = "conteudo_mensagem",
        X5:X39
    ) |>
    # remover colunas
    dplyr::select(-X3)


# ver estrutura
tidied_data |> dplyr::glimpse()
# tidied_data |> viewxl::view_in_xl()

# Clear mensage column ----------------------------------------------------

new_columns <- tidied_data |>
    # retirar linhas iniciais
    dplyr::slice(-c(1:4)) |>
    # remover usuário 'Maykon'
    dplyr::filter(usuario != "Maykon") |>
    dplyr::mutate(

        # remover pontuação
        usuario = stringr::str_replace_all(
            string = usuario,
            pattern = ":",
            replacement = ""
        ),

        # extrair tudo que que não seja o nome da usuária
        usuario_info_extra = dplyr::case_when(
            usuario != "Digníssima" ~ usuario,
            TRUE ~ NA_character_
        ),

        # limpar a coluna de usuario
        usuario = dplyr::case_when(
            usuario != "Digníssima" ~ "Digníssima",
            TRUE ~ usuario
        ),

        # criar coluna para identificar lantus
        lantus = dplyr::case_when(
            stringr::str_detect(
                string = conteudo_mensagem,
                pattern = "[lL]antus") ~ TRUE,
            TRUE ~ FALSE
        ),

        # criar coluna para identificar rápida
        rapida = dplyr::case_when(
            stringr::str_detect(
                string = conteudo_mensagem,
                pattern = "[Rr][áa]pida") ~ TRUE,
            TRUE ~ FALSE
        ),

        # remover underlines adicionais
        conteudo_mensagem = stringr::str_remove_all(
            string = conteudo_mensagem,
            pattern = "_ _ _+"
        ),

        # retirar espaços desnecessários
        conteudo_mensagem = stringr::str_squish(conteudo_mensagem),

        # extrair doses de insulina
        doses_rapida = stringr::str_extract_all(
            string = conteudo_mensagem,
            pattern = "[0-9]+uni|[0-9]+u|[0-9]+_u|[0-9]+n"
        ),

        # extrair horário
        horario_aplicacao = stringr::str_extract_all(
            string = conteudo_mensagem,
            pattern = "[0-9]+:[0-9]+|[0-9]+h"
        ),

        # extrair dia da aplicação
        data_aplicacao = stringr::str_extract_all(
            string = conteudo_mensagem,
            pattern = "[0-9]+/[0-9]+"
        ),

        # extrair qual foi a refeição
        refeicao = dplyr::case_when(

            # café da manhã
            stringr::str_detect(
                string = conteudo_mensagem,
                pattern = "[cC]af[eé]"
            ) ~ "Café da manhã",

            # almoço
            stringr::str_detect(
                string = conteudo_mensagem,
                pattern = "[aA]lmo"
            ) ~ "Almoço",

            # lanche da tarde
            stringr::str_detect(
                string = conteudo_mensagem,
                pattern = "[Ll]anche"
            ) ~ "Lanche da tarde",

            # janta
            stringr::str_detect(
                string = conteudo_mensagem,
                pattern = "[Jj]ant"
            ) ~ "Janta",

            TRUE ~ NA_character_

        ),

        # extrair a glicemia
        glicemia = stringr::str_extract_all(
            string = conteudo_mensagem,
            pattern = "[Gg]lice[_ ][0-9]+|[Gg]licemia[_ ][0-9]+|[gG]lice[0-9]+"
        )

    ) |>
    # corrigir colunas que saíram como output do extract_all
    tidyr::unnest(
        cols = c(
            "doses_rapida",
            "horario_aplicacao",
            "data_aplicacao",
            "glicemia"
        ),
        keep_empty = TRUE
    ) |>
    # remover itens que já identifiquei nas outras colunas
    dplyr::mutate(

        # remover refeições
        conteudo_clean = stringr::str_remove_all(
            string = conteudo_mensagem,
            pattern = "[cC]af[eé]|[aA]lmo[cç]o|[Jj]ant[aA]"
        ),

        # remover tipo de insulina
        conteudo_clean = stringr::str_remove_all(
            string = conteudo_clean,
            pattern = "[lL]antus|[rR][aá]pida"
        ),

        # remover horário de aplicação
        conteudo_clean = stringr::str_remove_all(
            string = conteudo_clean,
            pattern = "[0-9]+:[0-9]+|[0-9]+h"
        ),

        # remover unidades de insulina aplicada
        conteudo_clean = stringr::str_remove_all(
            string = conteudo_clean,
            pattern = "[0-9]+uni|[0-9]+u|[0-9]+_u|[0-9]+n"
        ),

        # remover glicemia
        conteudo_clean = stringr::str_remove_all(
            string = conteudo_clean,
            pattern = "[Gg]lice[_ ][0-9]+|[Gg]licemia[_ ][0-9]+|[gG]lice[0-9]+"
        ),

        # remover data
        conteudo_clean = stringr::str_remove_all(
            string = conteudo_clean,
            pattern = "[0-9]+/[0-9]+"
        )

    ) |>
    # extrair o restante de info da coluna 'conteudo_clean'
    dplyr::mutate(

        glicemia_validar = stringr::str_extract_all(
            string = conteudo_clean,
            pattern = "[0-9]+",
            simplify = FALSE
        ),

        # adicionar índice para cada linha
        indice = dplyr::row_number()

    ) |>
    # necessário para poder ajustar a coluna
    tidyr::unnest(glicemia_validar, keep_empty = TRUE)


new_columns |>
    # dplyr::glimpse()
    # analisando os dados da coluna de glicemia_validar foi possível limpar as
    # infos até a linha 308, antes dessa linha os números não são válidos para
    # a info de glicemia
    dplyr::mutate(
        # remover números que não fazem sentido tmb
        glicemia_validar_clean = dplyr::case_when(
            indice <= 309 ~ NA_character_,
            indice == 386 ~ NA_character_,
            indice == 508 ~ NA_character_,
            indice == 511 ~ NA_character_,
            indice == 733 ~ NA_character_,
            indice == 850 ~ NA_character_,
            TRUE ~ glicemia_validar
        ),

        # extrair apenas os números da coluna glicemia
        numeros_glicemia = stringr::str_extract_all(
            string = glicemia,
            pattern = "[0-9]+"
        ),

        # extrair apenas os números da coluna doses_rapida
        doses = stringr::str_extract_all(
            string = doses_rapida,
            pattern = "[0-9]+"
        )
    ) |>
    tidyr::unnest(
        cols = c("numeros_glicemia", "doses"),
        keep_empty = TRUE
    ) |>
    dplyr::select(
        -glicemia,
        -glicemia_validar,
        -conteudo_clean
    ) |>
    dplyr::mutate(
        glicemia = dplyr::case_when(
            is.na(numeros_glicemia) ~ glicemia_validar_clean,
            TRUE ~ numeros_glicemia
        )
    ) |>
    dplyr::select(
        -numeros_glicemia,
        -glicemia_validar_clean
    ) |>
    # dplyr::glimpse()

    viewxl::view_in_xl()


