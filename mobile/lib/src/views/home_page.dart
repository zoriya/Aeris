import 'dart:ui';

import 'package:flutter/material.dart';
import 'package:mobile/src/models/pipeline.dart';
import 'package:mobile/src/models/reaction.dart';
import 'package:mobile/src/models/service.dart';
import 'package:mobile/src/models/trigger.dart';
import 'package:mobile/src/widgets/aeris_page.dart';
import 'package:mobile/src/widgets/clickable_card.dart';
import 'package:mobile/src/widgets/loading_widget.dart';
import 'package:mobile/src/widgets/pipeline_card.dart';

/// Home Page
class HomePage extends StatefulWidget {
  const HomePage({Key? key}) : super(key: key);

  @override
  State<HomePage> createState() => _HomePageState();
}

class _HomePageState extends State<HomePage> {
  // Refresh/loading state
  bool loading = false;

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
    pipelines.sort((a, b) {
      if (a.enabled == b.enabled) {
        return b.trigger.last.compareTo(a.trigger.last);
      }
      return b.enabled ? 1 : -1;
    });
    ScrollController listController = ScrollController();
    var listView = ListView(
        controller: listController,
        padding:
            const EdgeInsets.only(top: 20, bottom: 20, left: 10, right: 10),
        children: [
          for (var pipeline in pipelines) PipelineCard(pipeline: pipeline),
          // Add button
          ClickableCard(
              color: Theme.of(context).colorScheme.secondary,
              body: Container(
                  child: Text(
                    "Create a Pipeline",
                    textAlign:
                        TextAlign.center,
                    style: TextStyle(
                        color: Theme.of(context).colorScheme.onSecondary,
                        fontSize: 20,
                        fontWeight: FontWeight.w600),
                  ),
                  width: double.infinity,
                  padding: const EdgeInsets.only(top: 20, bottom: 20)),
              onTap: () {
                print("Create new pipeline"); // TODO page transition
              })
        ]);
    return AerisPage(
        body: NotificationListener<ScrollEndNotification>(
      onNotification: (notification) {
        if (listController.position.atEdge) {
          if (listController.position.pixels == 0) {
            loading = true;
            print("Loading");
            setState(() {});
            Future.delayed(const Duration(seconds: 2)).then((_) => setState(() {
                  loading = false;
                  print("Loaded");
                }));
            // TODO Call API
          }
        }
        return true;
      },
      child: Stack(children: [
        listView,
        loading
            ? BackdropFilter(
                filter: ImageFilter.blur(sigmaX: 5.0, sigmaY: 5.0),
                child: Container())
            : Container(),
        loading ? const LoadingWidget() : Container()
      ]),
    ));
  }
}
