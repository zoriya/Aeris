import 'dart:ui';
import 'package:flutter/material.dart';
import 'package:mobile/src/providers/pipelines_provider.dart';
import 'package:mobile/src/widgets/aeris_page.dart';
import 'package:mobile/src/widgets/home_page_menu.dart';
import 'package:mobile/src/widgets/clickable_card.dart';
import 'package:mobile/src/widgets/home_page_sort_menu.dart';
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
    return Consumer<PipelineProvider>(
        builder: (context, provider, _) => AerisPage(
              actions: [
                HomePageSortMenu(
                  collectionProvider: provider,
                ),
                const HomePageMenu()
              ],
              body: NotificationListener<ScrollEndNotification>(
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
                        itemCount:
                            provider.pipelineCollection.pipelines.length + 1,
                        itemBuilder: (BuildContext context, int index) {
                          if (index ==
                              provider.pipelineCollection.pipelines.length) {
                            return ClickableCard(
                              color: Theme.of(context).colorScheme.secondary,
                              body: Padding(
                                padding:
                                    const EdgeInsets.only(top: 20, bottom: 20),
                                child: Text("Create a pipeline",
                                    textAlign: TextAlign.center,
                                    style: TextStyle(
                                        color: Theme.of(context)
                                            .colorScheme
                                            .onSecondary,
                                        fontSize: 20,
                                        fontWeight: FontWeight.w600)),
                              ),
                              onTap: () {
                                Navigator.of(context)
                                    .pushNamed('/pipeline/new');
                              },
                            );
                          }
                          return PipelineCard(
                              pipeline:
                                  provider.pipelineCollection.pipelines[index]);
                        },
                      ),
                      loading
                          ? BackdropFilter(
                              filter:
                                  ImageFilter.blur(sigmaX: 5.0, sigmaY: 5.0),
                              child: Container())
                          : Container(),
                      loading ? const LoadingWidget() : Container()
                    ],
                  )),
            ));
  }
}
