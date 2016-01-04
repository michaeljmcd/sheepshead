% Specification for Sheepshead Server 
% Michael McDermott

## Introduction ##

This document sketches out a REST-based API for a Sheepshead game server. A REST
API is advantageous because it is amenable to the construction of a web
application and yet still allows for other front-ends.

## User Management ##

### Adding a User / Assigning a New Nickname ###

The following endpoint will be used to establish a nickname for a given user and
is analagous to the idea of "connecting" to a server (this latter concept, while
frequent in games, is not truly applicable to a REST server).

**Endpoint:** `POST /user`

**Example Payload:** 

    { "nickname": "MilwaukeeMike" }

**Example Successful Return:** 

    200 OK 

    { "nickname": "MilwaukeeMike2", "ticket": "1234567890" }

The return value includes the nickname in case the user's requested name is
already extant and a new one is created. The ticket is to be included in all
subsequent requests to validate the user in question.

## Game Rooms ##

To retrieve a list of game rooms, the following endpoint may be used.

**Endpoint:** `GET /room`

**Example Payload:** *Not Applicable.*

**Example Successful Return:**

    200 OK

    { 
        "rooms" : [
            { 
                "id" : "12345",
                "name" : "Bob's Game", 
                "seats" : 5,
                "availableSeats" : 1
            }
        ]
    }

To retrieve information about a specific room, rather than a list of rooms, this
endpoint may be used instead.

**Endpoint:** `GET /room/<ID>`

**Example Payload:** *Not Applicable*

**Example Successful Return:**

    200 OK

    {
        "id" : "12345",
        "name" : "Bob's Game",
        "seats" : 5,
        "availableSeats: 1
    }

## Game Simulation ##

## Conclusion ##
