

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
    # substituir NAs
    dplyr::mutate(
        dplyr::across(
            .cols = dplyr::everything(),
            .fns = tidyr::replace_na,
            replace = " "
        )
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
tidied_data |> viewxl::view_in_xl()

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
        )
    ) |>

    viewxl::view_in_xl()

