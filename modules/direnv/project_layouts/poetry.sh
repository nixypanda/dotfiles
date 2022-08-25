layout_poetry() {
    if [[ ! -f pyproject.toml ]]; then
        log_error 'No pyproject.toml found. Use `poetry new` or `poetry init` to create one first.'
        exit 2
    fi

    local VENV=$(poetry env list --full-path | cut -d' ' -f1)
    if [[ -z $VENV || ! -d $VENV/bin ]]; then
        log_error 'No poetry virtual environment found. Use `poetry install` to create one first.'
        exit 2
    fi

    export VIRTUAL_ENV=$VENV
    export POETRY_ACTIVE=1
    PATH_add "$VENV/bin"
}
