CREATE TABLE IF NOT EXISTS survey (
    id bigserial PRIMARY KEY,
    active BOOLEAN NOT NULL DEFAULT 't',
    start_date TIMESTAMP NOT NULL,
    expires TIMESTAMP NOT NULL,
    created_on timestamptz NOT NULL DEFAULT now()
);
CREATE TYPE question_type as ENUM (
    'Checkbox',
    'LinearScale',
    'MultiChoice',
    'TextField',
    'EmailField'
);
CREATE TABLE IF NOT EXISTS survey_question (
    id bigserial PRIMARY KEY,
    title text NOT NULL,
    survey_id BIGINT NOT NULL REFERENCES survey (id),
    type question_type,
    choices text ARRAY,
    created_on timestamptz NOT NULL DEFAULT now()
);