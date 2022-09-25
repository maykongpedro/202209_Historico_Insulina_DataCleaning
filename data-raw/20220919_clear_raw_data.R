

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

tidied_data |>
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
            pattern = "[0-9]+uni|[0-9]+u|[0-9]+_u|[0-9]+n",
            simplify = TRUE
        ),

        # extrair horário
        horario_aplicacao = stringr::str_extract_all(
            string = conteudo_mensagem,
            pattern = "[0-9]+:[0-9]+|[0-9]+h",
            simplify = TRUE
        ),

        # extrair dia da aplicação
        data_aplicacao = stringr::str_extract_all(
            string = conteudo_mensagem,
            pattern = "[0-9]+/[0-9]+",
            simplify = TRUE
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
            pattern = "[Gg]lice[_ ][0-9]+|[Gg]licemia[_ ][0-9]+|[gG]lice[0-9]+",
            simplify = TRUE
        )

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
        )

    ) |>
    # extrair os números da coluna de conteúdo clena
    dplyr::mutate(

        glicemia_validar = stringr::str_extract_all(
            string = conteudo_clean,
            pattern = "[0-9]+",
            simplify = TRUE
        )

    ) |>
    viewxl::view_in_xl()


