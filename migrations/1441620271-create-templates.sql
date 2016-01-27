CREATE TABLE templates (
    template_id serial PRIMARY KEY,
    template    varchar(50) NOT NULL,
    icon        varchar(50),
    color_code  varchar(50),
    timestamp   timestamp default current_timestamp
);

CREATE TABLE templates_fields (
    field_id    int REFERENCES fields    (field_id) ON UPDATE CASCADE,
    template_id int REFERENCES templates (template_id) ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT templates_fields_pkey PRIMARY KEY (field_id, template_id)
);
