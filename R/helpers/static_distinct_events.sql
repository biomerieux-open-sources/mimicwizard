-- public.static_distinct_events source

CREATE MATERIALIZED VIEW public.static_distinct_events
TABLESPACE pg_default
AS SELECT cq.itemid,
    cq.label,
    cq.category,
    cq.linksto,
    cq.param_type
   FROM ( SELECT DISTINCT d_items.itemid,
            d_items.label,
            d_items.linksto,
            d_items.category,
            d_items.param_type
           FROM mimiciv_icu.d_items
          WHERE d_items.linksto::text = 'procedureevents'::text
        UNION ALL
         SELECT DISTINCT d_items.itemid,
            d_items.label,
            d_items.linksto,
            d_items.category,
            d_items.param_type
           FROM mimiciv_icu.d_items
          WHERE d_items.linksto::text = 'ingredientevents'::text
        UNION ALL
         SELECT DISTINCT d_items.itemid,
            d_items.label,
            d_items.linksto,
            d_items.category,
            d_items.param_type
           FROM mimiciv_icu.d_items
          WHERE d_items.linksto::text = 'inputevents'::text
        UNION ALL
         SELECT DISTINCT d_items.itemid,
            d_items.label,
            d_items.linksto,
            d_items.category,
            d_items.param_type
           FROM mimiciv_icu.d_items
          WHERE d_items.linksto::text = 'outputevents'::text
        UNION ALL
         SELECT DISTINCT d_items.itemid,
            d_items.label,
            d_items.linksto,
            d_items.category,
            d_items.param_type
           FROM mimiciv_icu.d_items
          WHERE d_items.linksto::text = 'datetimeevents'::text
        UNION ALL
         SELECT DISTINCT d_items.itemid,
            d_items.label,
            d_items.linksto,
            d_items.category,
            d_items.param_type
           FROM mimiciv_icu.d_items
          WHERE d_items.linksto::text = 'chartevents'::text
        UNION ALL
         SELECT DISTINCT d_labitems.itemid,
            d_labitems.label,
            'labevents'::text AS linksto,
            d_labitems.category,
            d_labitems.fluid AS param_type
           FROM mimiciv_hosp.d_labitems
        UNION ALL
         SELECT DISTINCT microbiologyresultsevents.itemid,
            microbiologyresultsevents.label,
            'microbiologyresultsevents'::text AS linksto,
            'Microbiology'::text AS category,
            NULL::character(1) AS param_type
           FROM microbiologyresultsevents) cq
WITH DATA;