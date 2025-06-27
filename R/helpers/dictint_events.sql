-- public.distinct_events source

CREATE OR REPLACE VIEW public.distinct_events
AS SELECT sde.itemid,
    sde.label,
    sde.category,
    sde.linksto,
    sde.param_type
   FROM static_distinct_events sde
UNION ALL
 SELECT dc.itemid,
    dc.label,
    'User imported'::character varying AS category,
    'customevents'::character varying AS linksto,
    dc.author AS param_type
   FROM d_customevents dc;
