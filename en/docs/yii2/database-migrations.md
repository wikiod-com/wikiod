---
title: "Database Migrations"
slug: "database-migrations"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Add Column
    public function up()
    {
        $this->addColumn('post', 'position', $this->integer());
    }


## Creating Migrations
    yii migrate/create <name>

The required name argument gives a brief description about the new migration. For example, if the migration is about creating a new table named news, you may use the name create_news_table and run the following command

    yii migrate/create create_news_table

## Migration File Example
    <?php

    use yii\db\Migration;

    class m150101_185401_create_news_table extends Migration
    {
    public function up()
    {

    }

    public function down()
    {
        echo "m101129_185401_create_news_table cannot be reverted.\n";

        return false;
    }

    /*
    // Use safeUp/safeDown to run migration code within a transaction
    public function safeUp()
    {
    }

    public function safeDown()
    {
    }
    */
    }

## Drop Table

    public function up()
    {
        $this->dropTable('post');
    }


## Create table fields right away
    yii migrate/create create_post_table --fields="title:string,body:text"

Generates:

    /**
     * Handles the creation for table `post`.
     */
    class m150811_220037_create_post_table extends Migration
    {
    /**
     * @inheritdoc
     */
    public function up()
    {
        $this->createTable('post', [
            'id' => $this->primaryKey(),
            'title' => $this->string(),
            'body' => $this->text(),
        ]);
    }

    /**
     * @inheritdoc
     */
    public function down()
    {
        $this->dropTable('post');
    }
    }

## Create Table
    public function up()
    {
        $this->createTable('post', [
            'id' => $this->primaryKey()
        ]);
    }


## Drop / Rename / Alter Column
    public function up()
    {
        $this->dropColumn('post', 'position');

        $this->renameColumn('post', 'owner_id', 'user_id');

        $this->alterColumn('post', 'updated', $this->timestamp()->notNull()->defaultValue('0000-00-00 00:00:00'));
    }

## Reverting Migrations
    yii migrate/down     # revert the most recently applied migration
    yii migrate/down 3   # revert the most 3 recently applied migrations

## Transactional Migrations
    public function safeUp()
    {
        $this->createTable('news', [
            'id' => $this->primaryKey(),
            'title' => $this->string()->notNull(),
            'content' => $this->text(),
        ]);

        $this->insert('news', [
            'title' => 'test 1',
            'content' => 'content 1',
        ]);
    }

    public function safeDown()
    {
        $this->delete('news', ['id' => 1]);
        $this->dropTable('news');
    }

An even easier way of implementing transactional migrations is to put migration code in the `safeUp()` and `safeDown()` methods. These two methods differ from `up()` and `down()` in that they are enclosed implicitly in a transaction. As a result, if any operation in these methods fails, all prior operations will be rolled back automatically.

## Migrating Multiple Databases
By default, migrations are applied to the same database specified by the db application component. If you want them to be applied to a different database, you may specify the db command-line option like shown below:

    yii migrate --db=db2



## Redoing Migrations
    yii migrate/redo        # redo the last applied migration
    yii migrate/redo 3      # redo the last 3 applied migrations

## Listing Migrations
    yii migrate/history     # showing the last 10 applied migrations
    yii migrate/history 5   # showing the last 5 applied migrations
    yii migrate/history all # showing all applied migrations

    yii migrate/new         # showing the first 10 new migrations
    yii migrate/new 5       # showing the first 5 new migrations
    yii migrate/new all     # showing all new migrations

## Modifying Migration History
    yii migrate/mark 150101_185401                      # using timestamp to specify the migration
    yii migrate/mark "2015-01-01 18:54:01"              # using a string that can be parsed by strtotime()
    yii migrate/mark m150101_185401_create_news_table   # using full name
    yii migrate/mark 1392853618                         # using UNIX timestamp

## Applying Migrations
    yii migrate

This command will list all migrations that have not been applied so far. If you confirm that you want to apply these migrations, it will run the up() or safeUp() method in every new migration class, one after another, in the order of their timestamp values. If any of the migrations fails, the command will quit without applying the rest of the migrations.

    yii migrate 3
    yii migrate/to 150101_185401                      # using timestamp to specify the migration
    yii migrate/to "2015-01-01 18:54:01"              # using a string that can be parsed by strtotime()
    yii migrate/to m150101_185401_create_news_table   # using full name
    yii migrate/to 1392853618                         # using UNIX timestamp




