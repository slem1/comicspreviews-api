CREATE SEQUENCE comicspreviews.seq_t_user_account_id_user_account;

ALTER SEQUENCE comicspreviews.seq_t_user_account_id_user_account OWNER TO comicspreviews;

CREATE TABLE comicspreviews.t_user_account(
    id_user_account bigint DEFAULT nextval('comicspreviews.seq_t_user_account_id_user_account'),
    username VARCHAR(256) NOT NULL CONSTRAINT uq_t_user_account_username UNIQUE,
    email VARCHAR(256) NOT NULL CONSTRAINT uq_t_user_account_email UNIQUE,
    password VARCHAR(256) NOT NULL,    
    enabled Bool NOT NULL,
    CONSTRAINT pk_id_t_user_account_id_user_account PRIMARY KEY (id_user_account)
)
WITH (
    OIDS = FALSE
);

CREATE INDEX ix_t_user_account_username ON comicspreviews.t_user_account (username);
CREATE INDEX ix_t_user_account_email ON comicspreviews.t_user_account (email);
