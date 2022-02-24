CREATE TABLE IF NOT EXISTS users (
	id SERIAL PRIMARY KEY,
	username VARCHAR(255) UNIQUE NOT NULL,
	password VARCHAR(255) NOT NULL,
	slug VARCHAR(255) UNIQUE NOT NULL,
	external_tokens JSONB[] NOT NULL
);

CREATE TABLE IF NOT EXISTS pipelines (
	id SERIAL PRIMARY KEY,
	name VARCHAR(255) NOT NULL,
	type VARCHAR(255) NOT NULL,
	params JSONB NOT NULL,
	user_id INTEGER NOT NULL,
	CONSTRAINT fk_user
		FOREIGN KEY (user_id)
			REFERENCES users(id)
);

CREATE TABLE IF NOT EXISTS reactions (
	id SERIAL PRIMARY KEY,
	type VARCHAR(255) NOT NULL,
	params JSONB NOT NULL,
	pipeline_id INTEGER NOT NULL,
	react_order INTEGER NOT NULL,
	CONSTRAINT fk_pipeline
		FOREIGN KEY (pipeline_id)
			REFERENCES pipelines(id)
				ON DELETE CASCADE
);
