import 'dart:ui';

import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:mobile/src/providers/pipelines_provider.dart';
import 'package:mobile/src/widgets/aeris_page.dart';
import 'package:mobile/src/widgets/clickable_card.dart';
import 'package:mobile/src/widgets/colored_clickable_card.dart';
import 'package:mobile/src/widgets/loading_widget.dart';
import 'package:mobile/src/widgets/pipeline_card.dart';
import 'package:provider/provider.dart';

/// Home Page
class HomePage extends StatefulWidget {
  const HomePage({Key? key}) : super(key: key);

  @override
  State<HomePage> createState() => _HomePageState();
}

class _HomePageState extends State<HomePage> {
  ///Refresh/loading state
  bool loading = false;

  @override
  Widget build(BuildContext context) {
    ScrollController listController = ScrollController();
    return AerisPage(
      body: Consumer<PipelineProvider>(
          builder: (context, provider, _) => NotificationListener<
                  ScrollEndNotification>(
              onNotification: (notification) {
                if (listController.position.atEdge) {
                  if (listController.position.pixels == 0) {
                    print("Loading");
                    setState(() {
                      loading = true;
                    });
                    Future.delayed(const Duration(seconds: 2))
                        .then((_) => setState(() {
                              loading = false;
                              print("Loaded");
                            }));
                    // TODO Call API
                  }
                }
                return true;
              },
              child: Stack(
                children: [
                  ListView.builder(
                    padding: const EdgeInsets.only(
                        top: 20, bottom: 20, left: 10, right: 10),
                    controller: listController,
                    itemCount: provider.pipelines.length + 1,
                    itemBuilder: (BuildContext context, int index) {
                      if (index == provider.pipelines.length) {
                        return ColoredClickableCard(
                          color: Theme.of(context).colorScheme.secondary,
                          text: "Create a pipeline",
                          onTap: () {
                            Navigator.of(context).pushNamed('/pipeline/new');
                          },
                        );
                      }
                      return PipelineCard(pipeline: provider.pipelines[index]);
                    },
                  ),
                  loading
                      ? BackdropFilter(
                          filter: ImageFilter.blur(sigmaX: 5.0, sigmaY: 5.0),
                          child: Container())
                      : Container(),
                  loading ? const LoadingWidget() : Container()
                ],
              ))),
    );
  }
}
