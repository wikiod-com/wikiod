---
title: "Dynamodb delete data over time"
slug: "dynamodb-delete-data-over-time"
draft: false
images: []
weight: 9999
type: docs
toc: true
---

Removing old data from dynamodb using a date attribute.

My use case: removing old data from dynamodb using a date attribute.

Important things to know:
- You can't query a table with using only range key attribute (date for example).
- You can only query a table using hash or hash+range key.
- You can't query a table using a hash key with '<' / '>' operations, only '='.

Possible Solutions:
- Scanning the whole table - this could be very costly
- My chosen solution - Defining an index with range key for the date and with a hash key that would be pretty decent such as the day of year.

Eventually batch delete the result set.

Notes:
Building the entity I was using the amazon dynamo annotations.
I was using DynamoDBQueryExpression to query, getting the result page with the defined Class object.

## cleanUpOldData
    public static void cleanUpOldData(AmazonDynamoDB amazonDynamoDB, String dynamoDBTablesPrefix, String tableName,
                                      String dateRangeField, String dateHashKey, String dateIndex, Class clazz) {
        log.info(String.format("Cleaning old data from table: %s", tableName));

        long cleanUpDateInMillis = (new Date()).getTime() - CLEAN_UP_TIME_MILLIS;
        SimpleDateFormat dateFormatter = new SimpleDateFormat(DYNAMO_DATE_FORMAT);
        final TimeZone utcTimeZone = TimeZone.getTimeZone("UTC");
        dateFormatter.setTimeZone(utcTimeZone);
        String cleanUpDate = dateFormatter.format(cleanUpDateInMillis);

        Calendar calendar = Calendar.getInstance(utcTimeZone);
        calendar.setTimeInMillis(cleanUpDateInMillis);

        final String dailyHashKey = String.format("%s_%s", calendar.get(Calendar.YEAR), calendar.get(Calendar.DAY_OF_YEAR));
        final String pastDayHashKey = String.format("%s_%s", calendar.get(Calendar.YEAR), calendar.get(Calendar.DAY_OF_YEAR)-1);

        final String fullTableName = dynamoDBTablesPrefix + "_" + tableName;
        final DynamoDBMapperConfig dbMapperConfig = new DynamoDBMapperConfig(new DynamoDBMapperConfig.TableNameOverride(fullTableName));
        DynamoDBMapper mapper = new DynamoDBMapper(amazonDynamoDB, dbMapperConfig);
        DynamoDBTableMapper dbTableMapper = mapper.newTableMapper(clazz);

        final QueryResultPage dailyResultPage = getDailyQueryResultPage(dateRangeField, dateHashKey, dateIndex, cleanUpDate, dailyHashKey, dbTableMapper);
        final QueryResultPage pastDayResultPage = getDailyQueryResultPage(dateRangeField, dateHashKey, dateIndex, cleanUpDate, pastDayHashKey, dbTableMapper);

        deleteOldData(dbTableMapper, dailyResultPage, pastDayResultPage);

        log.info(String.format("Completed cleaning old data from table: %s, %s items were deleted", tableName,
                dailyResultPage.getCount() + pastDayResultPage.getCount()));
    }

    private static QueryResultPage getDailyQueryResultPage(String dateRangeField, String dateHashKey, String dateIndex,
                                                           String cleanUpDate, String dayHashKey, DynamoDBTableMapper dbTableMapper) {
        HashMap<String, String > nameMap = new HashMap<>();
        nameMap.put("#date", dateRangeField);
        nameMap.put("#day", dateHashKey);
        HashMap<String, AttributeValue> valueMap = new HashMap<>();
        valueMap.put(":date", new AttributeValue().withS(cleanUpDate)) ;
        valueMap.put(":day", new AttributeValue().withS(dayHashKey));

        final DynamoDBQueryExpression dbQueryExpression = new DynamoDBQueryExpression()
                .withIndexName(dateIndex)
                .withConsistentRead(false)
                .withKeyConditionExpression("#day = :day and #date < :date")
                .withExpressionAttributeNames(nameMap)
                .withExpressionAttributeValues(valueMap);
        return dbTableMapper.query(dbQueryExpression);
    }

    private static void deleteOldData(DynamoDBTableMapper dbTableMapper, QueryResultPage dailyResultPage, QueryResultPage pastDayResultPage) {
        if (dailyResultPage.getCount() > 0) {
            dbTableMapper.batchDelete(dailyResultPage.getResults());
        }
        if (pastDayResultPage.getCount() > 0) {
            dbTableMapper.batchDelete(pastDayResultPage.getResults());
        }
    }

