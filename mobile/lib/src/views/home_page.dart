import 'dart:ui';

import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:mobile/src/models/pipeline.dart';
import 'package:mobile/src/models/reaction.dart';
import 'package:mobile/src/models/service.dart';
import 'package:mobile/src/models/trigger.dart';
import 'package:mobile/src/providers/pipelines_provider.dart';
import 'package:mobile/src/widgets/aeris_page.dart';
import 'package:mobile/src/widgets/clickable_card.dart';
import 'package:mobile/src/widgets/pipeline_card.dart';
import 'package:provider/provider.dart';

/// Home Page
class HomePage extends StatefulWidget {
  const HomePage({Key? key}) : super(key: key);

  @override
  State<HomePage> createState() => _HomePageState();
}

class _HomePageState extends State<HomePage> {
  @override
  Widget build(BuildContext context) {
    var trigger1 = Trigger(
        service: const Service.spotify(),
        action: "Play song",
        last: DateTime.now());
    var trigger3 = Trigger(
        service: const Service.discord(),
        action: "Send a message",
        last: DateTime.now());
    var trigger2 = Trigger(
        service: const Service.spotify(),
        action: "Play song",
        last: DateTime.parse("2022-01-01"));
    var reaction = const Reaction(
        service: Service.twitter(), parameters: {}, name: "Post a tweet");
    var pipeline1 = Pipeline(
        id: 10,
        name: "My Action",
        triggerCount: 10,
        enabled: true,
        parameters: {},
        trigger: trigger1,
        reactions: [reaction]);
    var pipeline2 = Pipeline(
        id: 10,
        name: "My very long action Action",
        triggerCount: 10,
        enabled: true,
        trigger: trigger2,
        parameters: {},
        reactions: [reaction, reaction]);
    var pipeline3 = Pipeline(
        id: 10,
        name: "Disabled",
        triggerCount: 10,
        enabled: false,
        trigger: trigger3,
        parameters: {},
        reactions: [reaction]);
    List<Pipeline> pipelines = [
      pipeline3,
      pipeline2,
      pipeline1,
      pipeline3,
      pipeline2,
      pipeline1,
      pipeline3,
      pipeline2,
      pipeline1
    ];

    PipelineProvider pipelineProvider = Provider.of<PipelineProvider>(context, listen: true);
    pipelineProvider.setPipelineProvider(pipelines);
    ScrollController listController = ScrollController();
    return AerisPage(
      body: Consumer<PipelineProvider>(
        builder: (context, provider, _) =>
          NotificationListener<ScrollEndNotification>(
            onNotification: (notification) {
              if (listController.position.atEdge) {
                if (listController.position.pixels == 0) {
                  print("Loading");
                  Future.delayed(const Duration(seconds: 2));
                  // TODO Call API
                }
              }
              return true;
            },
            child: Stack(
              children: [
                ListView.builder(
                  controller: listController,
                  itemCount: provider.pipelines.length + 1,
                  itemBuilder: (BuildContext context, int index) {
                    if (index == provider.pipelines.length) {
                      return ClickableCard(
                        color: Theme.of(context).colorScheme.secondary,
                        body: Padding(
                          padding: const EdgeInsets.only(top: 20, bottom: 20),
                          child: Text(
                            "Create a pipeline",
                            textAlign: TextAlign.center,
                            style: TextStyle(
                              color: Theme.of(context).colorScheme.onSecondary,
                              fontSize: 20,
                              fontWeight: FontWeight.w600
                            )
                          ),
                        ),
                        onTap: () {
                          if (kDebugMode) {
                            print("Create a pipeline");
                          }
                        },
                      );
                    }
                    return PipelineCard(
                      pipeline: provider.pipelines[index]
                    );
                  },
                ),
              ],
            )
          )
      ),
    );
  }
}
