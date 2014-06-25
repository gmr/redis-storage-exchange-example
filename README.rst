Redis Storage Exchange Plugin
======================================
This is a stripped down example RabbitMQ plugin and is not intended for production use.

The ``redis-storage-exchange`` example plugin for RabbitMQ will store messages published
through the exchange in Redis, using the routing key as the key in Redis and the message
body will be stored as the body.

This plugin is an example from RabbitMQ in Depth Chapter 14: Writting RabbitMQ Plugins.

Configuration
-------------

**Argument Based Configuration**

The default configuration is to connect to Redis on ``localhost:6379``. To connect
to a different Redis server, you can pass arguments when creating the exchange,
via policies, or configuration. The following table details the arguments to
pass when creating the ``x-redis-storage`` exchange.

+--------------+--------------------------------------+-----------+
| Setting      | Description                          | Data Type |
+==============+======================================+===========+
| x-host       | The Redis server hostname            | String    |
+--------------+--------------------------------------+-----------+
| x-port       | The port to connect on               | Number    |
+--------------+--------------------------------------+-----------+


**Policy Based Configuration**

To apply configuration via a policy, the following settings are available:

+-------------------------+--------------------------------------+-----------+
| Setting                 | Description                          | Data Type |
+=========================+======================================+===========+
| redis-storage-host      | The Redis server hostname            | String    |
+-------------------------+--------------------------------------+-----------+
| redis-storage-port      | The port to connect on               | Number    |
+-------------------------+--------------------------------------+-----------+


**Configuration in rabbitmq.config**

You can also change the default connection values in the ``rabbitmq.config`` file:

+--------------+---------------------------------+-----------+---------------+
| Setting      | Description                     | Data Type | Default Value |
+==============+=================================+===========+===============+
| host         | The Redis server hostname       | list      | "localhost"   |
+--------------+---------------------------------+-----------+---------------+
| port         | The port to connect on          | integer   | 5432          |
+--------------+---------------------------------+-----------+---------------+

*Exaple rabbitmq.config*

..  code-block:: erlang

    [{redis_storage_exchange,
      [
        {host: "localhost"},
        {port: 6379}
      ]}
    ].

Building
--------
Steps to custom build a version of the pgsql-listen exchange plugin:

.. code-block:: bash

    hg clone http://hg.rabbitmq.com/rabbitmq-public-umbrella
    cd rabbitmq-public-umbrella
    make co
    make BRANCH=rabbitmq_v3_3_3 up_c
    git clone https://github.com/gmr/reddit-wrapper.git
    git clone https://github.com/gmr/redis-storage-exchange-example.git
    cd redis-storage-exchange-example
    make
