---
title: "Many-to-many relationships"
slug: "many-to-many-relationships"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Simple Many To Many Relationship.
    class Person(models.Model):
        name = models.CharField(max_length=50)
        description = models.TextField()

    class Club(models.Model):
        name = models.CharField(max_length=50)
        members = models.ManyToManyField(Person)
    
Here we define a relationship where a club has many `Person`s and members and a Person can be a member of several different `Club`s.

Though we define only two models, django actually creates three tables in the database for us. These are `myapp_person`, `myapp_club` and myapp_club_members. Django automatically creates a unique index on `myapp_club_members(club_id,person_id)` columns.


## With a through model
    class Skill(models.Model):
        name = models.CharField(max_length=50)
        description = models.TextField()

    class Developer(models.Model):
        name = models.CharField(max_length=50)
        skills = models.ManyToManyField(Skill, through='DeveloperSkill')

    class DeveloperSkill(models.Model):
        """Developer skills with respective ability and experience."""

        class Meta:
            order_with_respect_to = 'developer'
            """Sort skills per developer so that he can choose which
            skills to display on top for instance.
            """
            unique_together = [
                ('developer', 'skill'),
            ]
            """It's recommended that a together unique index be created on
            `(developer,skill)`. This is especially useful if your database is
            being access/modified from outside django. You will find that such an
            index is created by django when an explicit through model is not
            being used.
            """


        ABILITY_CHOICES = [
            (1, "Beginner"),
            (2, "Accustomed"),
            (3, "Intermediate"),
            (4, "Strong knowledge"),
            (5, "Expert"),
        ]

        developer = models.ForeignKey(Developer, models.CASCADE)
        skill = models.ForeignKey(Skill, models.CASCADE)
        """The many-to-many relation between both models is made by the
        above two foreign keys.

        Other fields (below) store information about the relation itself.
        """

        ability = models.PositiveSmallIntegerField(choices=ABILITY_CHOICES)
        experience = models.PositiveSmallIntegerField(help_text="Years of experience.")

It's recommended that a together unique index be created on `(developer,skill)`. This is especially useful if your database is being access/modified from outside django. You will find that such an index is created by django when an explicit through model is not being used.


## Using ManyToMany Fields
We use this model from the first example:

    class Person(models.Model):
        name = models.CharField(max_length=50)
        description = models.TextField()
    
    class Club(models.Model):
        name = models.CharField(max_length=50)
        members = models.ManyToManyField(Person)

Add Tom and Bill to the Nightclub:

    tom = Person.objects.create(name="Tom", description="A nice guy")
    bill = Person.objects.create(name="Bill", description="Good dancer")
    
    nightclub = Club.objects.create(name="The Saturday Night Club")
    nightclub.members.add(tom, bill)

Who is in the club?

    for person in nightclub.members.all():
        print(person.name)

Will give you

    Tom
    Bill

