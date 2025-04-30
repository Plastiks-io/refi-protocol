import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import {
  faTwitter,
  faDiscord,
  faGithub,
} from "@fortawesome/free-brands-svg-icons";

const Footer = () => {
  return (
    <footer className="bg-black text-white py-4 sm:px-0 lg:px-20">
      <div className="container mx-auto flex justify-between items-center px-4">
        {/* Copyright */}
        <span className="text-sm">
          © 2025 Plastiks DApp. All rights reserved.
        </span>

        {/* Social Icons */}
        <div className="flex space-x-6">
          <a
            href="#"
            className="text-white hover:text-gray-400 transition"
            data-testid="icon-twitter"
          >
            <FontAwesomeIcon icon={faTwitter} size="lg" />
          </a>
          <a
            href="#"
            className="text-white hover:text-gray-400 transition"
            data-testid="icon-discord"
          >
            <FontAwesomeIcon icon={faDiscord} size="lg" />
          </a>
          <a
            href="#"
            className="text-white hover:text-gray-400 transition"
            data-testid="icon-github"
          >
            <FontAwesomeIcon icon={faGithub} size="lg" />
          </a>
        </div>
      </div>
    </footer>
  );
};

export default Footer;
