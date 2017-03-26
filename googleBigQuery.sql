SELECT owner_user_id, tagged
FROM (
SELECT owner_user_id, SPLIT(tags, '|') as tagged
FROM [bigquery-public-data:stackoverflow.posts_questions] 
WHERE tags <> "|"
)
GROUP BY  owner_user_id, tagged;
