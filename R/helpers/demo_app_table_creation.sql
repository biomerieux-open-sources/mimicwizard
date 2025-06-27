DROP SCHEMA IF EXISTS public CASCADE;
CREATE SCHEMA public;

CREATE TABLE public.cohort (
	cohort_id float8 NULL,
	subject_id float8 NULL,
	hadm_id float8 NULL,
	stay_id float8 NULL
);

CREATE TABLE public.customevents (
	subject_id float8 NULL,
	hadm_id float8 NULL,
	stay_id float8 NULL,
	itemid float8 NULL,
	value varchar(20) NULL,
	charttime timestamptz NULL,
	valueuom text NULL
);

CREATE SEQUENCE public.d_cohorts_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 2147483647
	START 1
	CACHE 1
	NO CYCLE;

	CREATE SEQUENCE public.d_customevents_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 2147483647
	START 10000
	CACHE 1
	NO CYCLE;

CREATE SEQUENCE public.users_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 2147483647
	START 1
	CACHE 1
	NO CYCLE;

CREATE TABLE public.d_cohorts (
	cohort_id int4 DEFAULT nextval('d_cohorts_seq'::regclass) NOT NULL,
	cohort_name varchar NULL,
	cohort_description text NULL
);

CREATE TABLE public.d_customevents (
	itemid int4 DEFAULT nextval('d_customevents_seq'::regclass) NOT NULL,
	"label" varchar NULL,
	abbreviation varchar NULL,
	author varchar NULL,
	total_row numeric NULL
);

CREATE TABLE public.users (
	user_id int4 DEFAULT nextval('users_seq'::regclass) NOT NULL,
	user_name varchar NULL,
	user_description varchar NULL,
	user_itemids varchar NULL,
	user_cohorts varchar NULL
);
