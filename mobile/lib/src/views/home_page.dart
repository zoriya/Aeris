import 'package:flutter/material.dart';
import 'package:mobile/src/models/pipeline.dart';
import 'package:mobile/src/models/reaction.dart';
import 'package:mobile/src/models/service.dart';
import 'package:mobile/src/models/trigger.dart';
import 'package:mobile/src/widgets/aeris_page.dart';
import 'package:mobile/src/widgets/pipeline_card.dart';

/// Home Page
class HomePage extends StatelessWidget {
  const HomePage({Key? key}) : super(key: key);

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
    var reaction = const Reaction(service: Service.twitter(), parameters: {});
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
    return AerisPage(
        body: Column(children: [
      PipelineCard(pipeline: pipeline1),
      PipelineCard(pipeline: pipeline2),
      PipelineCard(pipeline: pipeline3)
    ]));
  }
}
