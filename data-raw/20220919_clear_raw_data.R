

# Read data ---------------------------------------------------------------

raw_data <- readr::read_table(
    file = "data-raw/conversa_whats.txt",
    col_names = FALSE,
    # skip = 4
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
