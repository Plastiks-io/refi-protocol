// src/components/admin/Setting.tsx
import { useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { AppDispatch, RootState } from "@/redux/store";
import { Loader2, X, Settings, Users, Loader } from "lucide-react";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faBoxArchive } from "@fortawesome/free-solid-svg-icons";
import axios from "axios";
import { toast } from "sonner";
import { removeArchivedRoadmap } from "@/redux/archivedRoadmapSlice";
import { addRoadmap } from "@/redux/roadmapSlice";
import { User2 } from "@/assets/icons";
import { removeAdmin } from "@/redux/adminSlice";
import AddAdminPopup from "./AddAdminPopup";

const SettingsComponent = () => {
  const dispatch = useDispatch<AppDispatch>();

  // function to restore archived roadmap
  const restoreRoadmap = async (roadmap: any) => {
    try {
      setLoading(true);
      const url = import.meta.env.VITE_SERVER_URL;
      const apiUrl = `${url}/roadmap/restore/${roadmap.id}`;
      const response = await axios.post(apiUrl, null, {
        withCredentials: true,
      });

      // also remove from redux state
      dispatch(removeArchivedRoadmap(roadmap.id));
      // insert into active roadmaps
      dispatch(addRoadmap(roadmap));

      console.log(response.data);
      toast.success("Roadmap restored successfully");
      setLoading(false);
    } catch (error) {
      toast.error("Failed to restore roadmap");
      console.error("Error restoring roadmap:", error);
      setLoading(false);
    }
  };

  // function to completely delete archived roadmap
  const deleteRoadmap = async (id: string) => {
    try {
      setLoading(true);
      const url = import.meta.env.VITE_SERVER_URL;
      const apiUrl = `${url}/roadmap/archived/${id}`;
      const response = await axios.delete(apiUrl, {
        withCredentials: true,
      });

      // also remove from redux state
      dispatch(removeArchivedRoadmap(id));
      console.log(response.data);
      toast.success("Roadmap restored successfully");
      setLoading(false);
    } catch (error) {
      toast.error("Failed to delete roadmap");
      console.error("Error deleting roadmap:", error);
      setLoading(false);
    }
  };

  const removeAdmins = async (id: string) => {
    try {
      setLoading(true);
      const url = import.meta.env.VITE_SERVER_URL;
      const apiUrl = `${url}/admin/${id}`;
      const response = await axios.delete(apiUrl, {
        withCredentials: true,
      });

      // also remove from redux state
      dispatch(removeAdmin(id));
      console.log(response.data);
      toast.success("Admin removed successfully");
      setLoading(false);
    } catch (error) {
      toast.error("Failed to remove admin");
      console.error("Error removing admin:", error);
      setLoading(false);
    }
  };

  // Get Archived roadmaps from redux state
  const {
    roadmaps: archivedRoadmaps,
    loading: loadingArchived,
    error: errorArchived,
  } = useSelector((state: RootState) => state.archivedRoadmaps);

  const { admins: adminList } = useSelector((state: RootState) => state.admin);

  const { walletAddress } = useSelector((state: RootState) => state.wallet);

  // State for add admin popup
  const [showAddAdmin, setShowAddAdmin] = useState(false);
  const [activeTab, setActiveTab] = useState<"archived" | "admin">("archived");
  const [loading, setLoading] = useState(false);

  return (
    <div className="p-6 md:p-10 bg-[#ffffff] text-[#1F2937] min-h-[80vh]">
      <div className="flex gap-2">
        <Settings width={32} height={32} />
        <div>
          <h1 className="text-3xl font-bold mb-1 flex items-center space-x-2">
            Settings
          </h1>
          <p className="text-gray-600 my-4">
            Manage archives, permissions and security
          </p>
        </div>
      </div>

      {/* Tabs + Add Admin */}
      <div className="flex mb-4 items-center">
        {/* Tab 1 */}
        <button
          className={`flex items-center space-x-2 mr-5 pb-2 cursor-pointer ${
            activeTab === "archived"
              ? "border-b-3 border-[#082FB9] text-[#082FB9]"
              : "text-[#0D0D0D]"
          }`}
          onClick={() => setActiveTab("archived")}
        >
          <FontAwesomeIcon icon={faBoxArchive} size="lg" />
          <span className="py-2 font-medium transition-colors text-xl">
            Archived Roadmaps
          </span>
        </button>

        {/* Tab 2 */}
        <button
          className={`flex items-center space-x-2 pb-2 cursor-pointer ${
            activeTab === "admin"
              ? "border-b-3 border-[#082FB9] text-[#082FB9]"
              : "text-[#0D0D0D]"
          }`}
          onClick={() => setActiveTab("admin")}
        >
          <Users size={20} />
          <span className="py-2 font-medium transition-colors text-xl">
            Admin Permissions
          </span>
        </button>

        {/* Spacer + Add Admin */}
        {activeTab === "admin" && (
          <button
            className="ml-auto flex items-center gap-1.5 bg-[#082FB9] text-white font-semibold px-4 py-2.5 rounded-full cursor-pointer"
            onClick={() => setShowAddAdmin(true)}
          >
            <img src={User2} alt="user plus" className="w-5 h-5" />
            <span>Add Admin</span>
          </button>
        )}
      </div>

      {/* Content */}
      {loadingArchived ? (
        <div className="flex justify-center items-center h-32" role="status">
          <Loader2 className="animate-spin w-10 h-10 text-gray-600" />
        </div>
      ) : errorArchived ? (
        <div className="flex items-center gap-2 text-red-600">
          <X className="w-5 h-5" />
          <span>Failed to load roadmaps. Please try again later.</span>
        </div>
      ) : (
        <>
          {activeTab === "archived" && (
            <div className="mt-4 border border-[#E5E5E5] rounded-2xl">
              <h2 className="text-xl font-semibold text-center text-[#1F2937] p-4">
                Archived Roadmaps
              </h2>
              <div className="overflow-x-auto">
                <table className="min-w-full table-fixed text-left min-h-20">
                  <thead className="bg-gray-100">
                    <tr className="text-[#404040] text-md">
                      <th className="px-4 py-3 font-semibold w-1/4 rounded-tl-2xl">
                        Roadmap
                      </th>
                      <th className="px-4 py-3 font-semibold w-1/4">Entity</th>
                      <th className="px-4 py-3 font-semibold w-1/4">
                        Date Archived
                      </th>
                      <th className="px-4 py-3 font-semibold w-1/4 rounded-tr-2xl">
                        Actions
                      </th>
                    </tr>
                    {/* spacer row */}
                    <tr>
                      <td colSpan={4}>
                        <div className="h-0.5 bg-[#E5E7EB]" />
                      </td>
                    </tr>
                  </thead>

                  <tbody>
                    {/* spacer row */}
                    <tr>
                      <td colSpan={4}>
                        <div className="h-1" />
                      </td>
                    </tr>

                    {archivedRoadmaps.map((roadmap, idx) => {
                      const isLast = idx === archivedRoadmaps.length - 1;
                      return (
                        <tr key={idx} className="hover:bg-gray-50 h-20">
                          <td
                            className={`px-4 py-3 text-[#404040] text-md font-semibold ${
                              isLast ? "rounded-bl-2xl" : ""
                            }`}
                          >
                            {roadmap.roadmapName}
                          </td>
                          <td className="px-4 py-3 text-sm text-gray-800">
                            {roadmap.preId}
                          </td>
                          <td className="px-4 py-3 text-sm text-gray-800">
                            {new Date(roadmap.dateArchived).toLocaleDateString(
                              "en-US",
                              {
                                year: "numeric",
                                month: "short",
                                day: "numeric",
                              }
                            )}
                          </td>
                          <td
                            className={`px-4 py-3 text-sm font-semibold ${
                              isLast ? "rounded-br-2xl" : ""
                            }`}
                          >
                            {loading ? (
                              <Loader className="animate-spin text-[#082FB9]" />
                            ) : (
                              <button
                                className="text-[#082FB9] border-b-2 mr-4 cursor-pointer"
                                onClick={() => restoreRoadmap(roadmap)}
                              >
                                Restore
                              </button>
                            )}
                            <button
                              className="text-[#DE0923] border-b-2 cursor-pointer"
                              onClick={() => deleteRoadmap(roadmap.id)}
                            >
                              Delete
                            </button>
                          </td>
                          <td colSpan={4}>
                            <div className="h-0.5 bg-[#E5E7EB]" />
                          </td>
                        </tr>
                      );
                    })}
                  </tbody>
                </table>
              </div>
            </div>
          )}

          {activeTab === "admin" && (
            <div className="mt-4 border border-[#E5E5E5] rounded-2xl">
              <h2 className="text-xl font-semibold text-center text-[#1F2937] p-4">
                Admin Permissions
              </h2>
              <div className="overflow-x-auto">
                <table className="min-w-full table-fixed text-left min-h-20">
                  <thead className="bg-gray-100">
                    <tr className="text-[#404040] text-md">
                      <th className="px-4 py-3 font-semibold w-1/4 rounded-tl-2xl">
                        Wallet Address
                      </th>
                      <th className="px-4 py-3 font-semibold w-1/4">
                        Date Invited
                      </th>
                      <th className="px-4 py-3 font-semibold w-1/4 rounded-tr-2xl">
                        Actions
                      </th>
                    </tr>
                    {/* spacer row */}
                    <tr>
                      <td colSpan={4}>
                        <div className="h-0.5 bg-[#E5E7EB]" />
                      </td>
                    </tr>
                  </thead>

                  <tbody>
                    {/* spacer row */}
                    <tr>
                      <td colSpan={4}>
                        <div className="h-1" />
                      </td>
                    </tr>
                    {adminList.map((admin, idx) => {
                      const isLast = idx === adminList.length - 1;
                      const isSelfSuperAdmin =
                        admin.role === "SUPER_ADMIN" ||
                        admin.address === walletAddress;

                      return (
                        <tr key={idx} className="hover:bg-gray-50 h-20">
                          <td
                            className={`px-4 py-3 text-[#404040] text-md font-semibold ${
                              isLast ? "rounded-bl-2xl" : ""
                            }`}
                          >
                            {admin.address.slice(0, 6)}...
                            {admin.address.slice(-4)}
                          </td>
                          <td className="px-4 py-3 text-sm text-gray-800">
                            {new Date(admin.createdAt).toLocaleDateString(
                              "en-US",
                              {
                                year: "numeric",
                                month: "short",
                                day: "numeric",
                              }
                            )}
                          </td>
                          <td
                            className={`px-4 py-3 text-sm font-semibold ${
                              isLast ? "rounded-br-2xl" : ""
                            }`}
                          >
                            {loading ? (
                              <Loader className="animate-spin text-[#082FB9]" />
                            ) : (
                              <button
                                className={`mr-4 ${
                                  isSelfSuperAdmin
                                    ? "text-gray-400 cursor-not-allowed"
                                    : "text-[#DE0923] cursor-pointer border-b-2"
                                }`}
                                disabled={isSelfSuperAdmin}
                                onClick={() => {
                                  if (isSelfSuperAdmin) {
                                    return;
                                  }
                                  removeAdmins(admin.id);
                                }}
                              >
                                {isSelfSuperAdmin
                                  ? "You cannot remove yourself or super admin"
                                  : "Remove"}
                              </button>
                            )}
                          </td>
                        </tr>
                      );
                    })}
                  </tbody>
                </table>
              </div>
            </div>
          )}
          <AddAdminPopup
            isOpen={showAddAdmin}
            onClose={() => setShowAddAdmin(false)}
            existingAdmins={adminList}
            walletAddress={walletAddress}
          />
        </>
      )}
    </div>
  );
};

export default SettingsComponent;
